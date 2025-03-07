;;; gptel-openai-assistant.el --- openai assistant interface for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025 Shariff AM Faleel

;; Author: Shariff AM Faleel
;; Package-Requires: ((emacs "28") (gptel "0.9.7"))
;; Version: 0.1-pre
;; Homepage: https://github.com/ahmed-shariff/gptel-openai-assistant
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; openai assistant interface for gptel

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'url)
(require 'map)

;; Openai assistant api ******************************************************************
(defvar-local gptel-openai-assistant-thread-id nil)

;; TODO: Get this from api?
(defvar-local gptel-openai-assistant-assistant-id (gethash 'openai-assistant-id configurations))

;; Helper functions **********************************************************************
(defvar url-http-end-of-headers)
(defun gptel-openai-assistant--url-retrive (method data url info callback)
  "Get data from URL with DATA using METHOD (POST/GET).
INFO is info from gptel
CALLBACK is called with the response from calling url-retrive."
  (let ((url-request-method method)
        (url-request-extra-headers
         (append `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))
                   ("OpenAI-Beta" . "assistants=v2"))))
        (url-request-data data))
    (when gptel-log-level               ;logging
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             url-request-extra-headers))
                    "request headers"))
      (when url-request-data
        (gptel--log url-request-data "request body")))
    (url-retrieve (cl-typecase url
                    (string url)
                    (function (funcall url))
                    (t (error "Unknown value for url (%s) in step" url)))
                  (lambda (_)
                    (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                 (gptel-openai-assistant--url-parse-response))
                                (buf (current-buffer)))
                      (plist-put info :http-status http-status)
                      (plist-put info :status http-msg)
                      (when error
                        (plist-put info :error error))
                      (with-current-buffer (plist-get info :buffer)
                        (funcall callback response))
                      (kill-buffer buf)
                      ))
                  nil t t)))

;; copied from `gptel--url-parse-response' beacuse we don't want the following:
;; (gptel--parse-response backend response proc-info)
(defun gptel-openai-assistant--url-parse-response ()
  "Parse response from url-retrive."
  (when gptel-log-level             ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq gptel-log-level 'debug)
        (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (gptel--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (http-status
             (save-match-data
               (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                    (match-string 1 http-msg))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (gptel--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list response
              http-status http-msg))
       ((plist-get response :error)
        (list nil http-status http-msg (plist-get response :error)))
       ((eq response 'json-read-error)
        (list nil http-status (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))


;; Core function to work with gptel ******************************************************
(cl-defstruct (gptel-openai-assistant
               (:constructor gptel-openai--make-assistant)
               (:copier nil)
               (:include gptel-openai))
  messages-data)

(defun gptel-openai-assistant-start-thread (info &optional callback)
  "Use the threads endpoint to start new thread.
Set the `gptel-openai-assistant-thread-id' of the buffer.
INFO is the info plist from gptel.
CALLBACK is invoked without any args after successfully creating a thread."
  (gptel-openai-assistant--url-retrive
   "POST" nil "https://api.openai.com/v1/threads"
   info
   (lambda (response)
     (with-current-buffer
         (plist-get info :buffer)
       (setq-local gptel-openai-assistant-thread-id (plist-get response :id))
       (when callback (funcall callback))))))

(defun gptel-openai-assistant-add-message (info callback)
  "Use the messages endpoint to start new thread.
Needs the `gptel-openai-assistant-thread-id' of the buffer to be set.
INFO is the info plist from gptel.
CALLBACK is invoked without any args after successfully creating a thread."
  (gptel-openai-assistant--url-retrive
   "POST"
   (encode-coding-string
    (gptel--json-encode (gptel-openai-assistant-messages-data (plist-get info :backend)))
    'utf-8)
   (with-current-buffer
       (plist-get info :buffer)
     (if gptel-openai-assistant-thread-id
         (format "https://api.openai.com/v1/threads/%s/messages"
                 gptel-openai-assistant-thread-id)
       (user-error "No thread in current buffer to add messages!")))
   info
   (lambda (response)
     (when callback (funcall callback)))))

(cl-defmethod gptel--inject-prompt ((_ gptel-openai-assistant) data new-prompt &optional _position)
  ;; The tool calls are handled differently in oai
  ;; FIXME: What are the other use cases for inject?
  (let ((prompts (plist-get data :tool_outputs)))
    (plist-put data :tool_outputs (vconcat prompts new-prompt))))

(cl-defmethod gptel--parse-tool-results ((_ gptel-openai-assistant) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  ;; (declare (side-effect-free t))
  (mapcar
   (lambda (tool-call)
     (list
      :output (plist-get tool-call :result)
      :tool_call_id (plist-get tool-call :id)))
   tool-use))

(cl-defmethod gptel--request-data ((backend gptel-openai-assistant) prompts)
  (setf (gptel-openai-assistant-messages-data backend)
        (list :role "user"
              :content
              `[(:type "text"
                       :text ,(plist-get (car (last prompts)) :content))]))
  (if (and gptel-stream gptel-use-curl
           (gptel-backend-stream backend))
      (let ((prompts-plist
             `(:assistant_id ,gptel-openai-assistant-assistant-id
                             :stream ,(or (and gptel-stream gptel-use-curl
                                               (gptel-backend-stream backend))
                                          :json-false))))
        (when gptel-temperature
          (plist-put prompts-plist :temperature gptel-temperature))
        (when gptel-max-tokens
          (plist-put prompts-plist (if (memq gptel-model '(o1-preview o1-mini))
                                       :max_completion_tokens :max_tokens)
                     gptel-max-tokens))
        ;; Merge request params with model and backend params.
        (gptel--merge-plists
         prompts-plist
         (gptel-backend-request-params backend)
         (gptel--model-request-params  gptel-model)))
    `(:metadata nil)))

(cl-defmethod gptel--parse-response ((_ gptel-openai-assistant) response info)
  (string-join
   (mapcar
    (lambda (content)
      (let ((str (map-nested-elt content '(:text :value))))
        (mapcar
         (lambda (annotation)
           (setf str
                 (string-replace
                  (plist-get annotation :text)
                  (format "[file_citation:%s]" (map-nested-elt annotation '(:file_citation :file_id)))
                  str)))
         (map-nested-elt content '(:text :annotations)))
        str))
      (plist-get response :content))
     " "))

(cl-defmethod gptel-curl--parse-stream ((_ gptel-openai-assistant) info)
  (let* ((content-strs))
    (condition-case err
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                ;; The stream has ended, so we do the following thing (if we found tool calls)
                ;; - pack tool calls into the messages prompts list to send (INFO -> :data -> :messages)
                ;; - collect tool calls (formatted differently) into (INFO -> :tool-use)
                (when-let* ((tool-use (plist-get info :tool-use))
                            (args (apply #'concat (nreverse (plist-get info :partial_json))))
                            (func (plist-get (car tool-use) :function)))
                  (plist-put func :arguments args) ;Update arguments for last recorded tool
                  (plist-put info :data (list :stream (plist-get info :stream)))
                  (cl-loop
                   for tool-call in tool-use ; Construct the call specs for running the function calls
                   for spec = (plist-get tool-call :function)
                   collect (list :id (plist-get tool-call :id)
                                 :name (plist-get spec :name)
                                 :args (ignore-errors (gptel--json-read-string
                                                       (plist-get spec :arguments))))
                   into call-specs
                   finally (plist-put info :tool-use call-specs)))
              (when-let* ((response (gptel--json-read)))
                (when (and (null (plist-get info :openai-assistant-run-id))
                           (string-equal (plist-get response :object) "thread.run"))
                  (plist-put info :openai-assistant-run-id (plist-get response :id)))
                (if-let* ((content (map-nested-elt response '(:delta :content 0 :text :value)))
                          ((not (eq content :null))))
                    (if-let* ((annotations (map-nested-elt
                                            response '(:delta :content 0 :text :annotations)))
                              (_ (length> annotations 0)))
                        (cl-loop for annotation across annotations
                                 for file-id = (map-nested-elt
                                                annotation '(:file_citation :file_id))
                                 if file-id
                                 do (push (format "[file_citation:%s]" file-id content) content-strs))
                      (push content content-strs))
                  ;; No text content, so look for tool calls
                  (when-let* ((tool-call (map-nested-elt response '(:delta :step_details :tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (plist-get func :name) ;new tool block begins
                        (progn
                          (when-let* ((partial (plist-get info :partial_json)))
                            (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool-call :function)))
                              (plist-put prev-func :arguments ;update args for old tool block
                                         (apply #'concat (nreverse (plist-get info :partial_json)))))
                            (plist-put info :partial_json nil)) ;clear out finished chain of partial args
                          ;; Start new chain of partial argument strings
                          (plist-put info :partial_json (list (plist-get func :arguments)))
                          ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                      ;; old tool block continues, so continue collecting arguments in :partial_json 
                      (push (plist-get func :arguments) (plist-get info :partial_json)))))))))
      ((json-parse-error json-end-of-file search-failed)
       (goto-char (match-beginning 0)))
      (error
       (signal (car err) (cdr err))))
    (apply #'concat (nreverse content-strs))))

(defun gptel-openai-assistant--handle-await (fsm)
  (let* ((info (gptel-fsm-info fsm))
         (await-manual-state
          (or (plist-get info :openai-assistant-await)
              (let ((history (plist-get info :history)))
                (when (and
                       (eq (car history) 'INIT)
                       (null (cdr history)))
                  :send-message)))))
    (plist-put info :openai-assistant-await nil)
    (when (gptel-openai-assistant-p (plist-get info :backend))
      (with-current-buffer (plist-get (gptel-fsm-info fsm) :buffer)
        (gptel--update-status " Waiting..." 'warning))
      ;; This tells us await manual is happening from after the init
      (pcase await-manual-state
        (:send-message
         (cl-labels ((send-message ()
                       (gptel-openai-assistant-add-message
                        info
                        (lambda ()
                          (if (plist-get info :stream)
                              (plist-put info :openai-assistant-wait t)
                            (plist-put info :openai-assistant-await :openai-assistant-runs))
                          (gptel--fsm-transition fsm)))))
           (if gptel-openai-assistant-thread-id
               (send-message)
             (gptel-openai-assistant-start-thread
              info
              (lambda ()
                (if (plist-get info :error)
                    (gptel--fsm-transition fsm)
                  (send-message)))))))
        (:openai-assistant-runs
         (gptel-openai-assistant--url-retrive
          "POST"
          (encode-coding-string
           (gptel--json-encode `(:assistant_id ,gptel-openai-assistant-assistant-id))
           'utf-8)
          (with-current-buffer
              (plist-get info :buffer)
            (if gptel-openai-assistant-thread-id
                (format "https://api.openai.com/v1/threads/%s/runs"
                        gptel-openai-assistant-thread-id)
              (user-error "No thread in current buffer to add messages!")))
          info
          (lambda (response)
            (unless (plist-get info :error)
              (plist-put info :openai-assistant-run-id (plist-get response :id))
              (plist-put info :openai-assistant-await :openai-assistant-runs-completed-p)
              (plist-put info :openai-assistant-delay 0.5))
            (gptel--fsm-transition fsm))))
        (:openai-assistant-runs-completed-p
         (gptel-openai-assistant--url-retrive
          "GET"
          nil
          (with-current-buffer
              (plist-get info :buffer)
            (if gptel-openai-assistant-thread-id
                (format "https://api.openai.com/v1/threads/%s/runs/%s"
                        gptel-openai-assistant-thread-id
                        (plist-get info :openai-assistant-run-id))
              (user-error "No thread in current buffer to add messages!")))
          info
          (lambda (response)
            (unless (plist-get info :error)
              (pcase (plist-get response :status)
                ((or "in_progress" "queued")
                 (plist-put info :openai-assistant-await :openai-assistant-runs-completed-p)
                 (plist-put info :openai-assistant-delay 0.5))
                ("completed"
                 (plist-put info :openai-assistant-await :openai-assistant-list-messages))
                (status (error "Unhandle state for run %s" status))))
            (gptel--fsm-transition fsm))))
        (:openai-assistant-list-messages
         (gptel-openai-assistant--url-retrive
          "GET"
          nil
          (with-current-buffer
              (plist-get info :buffer)
            (if gptel-openai-assistant-thread-id
                (format "https://api.openai.com/v1/threads/%s/messages?run_id=%s"
                        gptel-openai-assistant-thread-id
                        (plist-get info :openai-assistant-run-id))
              (user-error "No thread in current buffer to add messages!")))
          info
          (lambda (response)
            (unless (plist-get info :error)
              (plist-put info :openai-assistant-message-id (map-nested-elt response '(:data 0 :id)))
              (plist-put info :openai-assistant-wait t))
            (gptel--fsm-transition fsm))))
        (_ (error "Unknown await state %s" await-manual-state))))))

;;;###autoload
(defun gptel-make-openai-assistant ()
  "Create a openai-assistant backend."
  (gptel-openai--make-assistant
   :name "gptel-openai-assistant"
   :host "api.openai.com"
   :key 'gptel-api-key
   :models (gptel--process-models gptel--openai-models)
   :header (lambda () (when-let (key (gptel--get-api-key))
                        `(("Authorization" . ,(concat "Bearer " key))
                          ("OpenAI-Beta" . "assistants=v2"))))
   :protocol "https"
   :endpoint "/v1/threads/thread_id/runs"
   :url (lambda ()
          (if gptel-openai-assistant-thread-id
              (format "https://api.openai.com/v1/threads/%s/runs" gptel-openai-assistant-thread-id)
            (user-error "No thread in current buffer to add messages!")))
   :stream t))

;;;###autoload
(defun gptel-openai-assistant-create-new-thread ()
  "Create a new thread in the current buffer."
  (interactive)
  (gptel-openai-assistant-start-thread `(:buffer ,(buffer-name))))

;; Modify gptel vars *********************************************************************

(setf (alist-get "openai-assistant" gptel--known-backends
                 nil nil #'equal)
      (gptel-make-openai-assistant))

(defun gptel-openai-assistant--backend-is-oaia-p (info)
  "Check if backend is openai-assistant."
  (gptel-openai-assistant-p (plist-get info :backend)))

(defun gptel-openai-assistant--init-to-await (info)
  "If in first INIT, move to AWAIT"
  (and (gptel-openai-assistant--backend-is-oaia-p info)
       ;; If :history is not set or empty, means state is at INIT
       (null (plist-get info :history))))

(defun gptel-openai-assistant--wait-again-p (info)
  "Check if the fsm should WAIT."
  (and (gptel-openai-assistant--backend-is-oaia-p info)
       (plist-get info :openai-assistant-wait)))

(defun gptel-openai-assistant--delay-p (info)
  "Test fsm transition to DELAY."
  (and (gptel-openai-assistant--backend-is-oaia-p info)
       (plist-get info :openai-assistant-delay)))

(defun gptel-openai-assistant--await-p (info)
  "Test fsm transition to AWAIT."
  (and (gptel-openai-assistant--backend-is-oaia-p info)
       (plist-get info :openai-assistant-await)))

(defun gptel-openai-assistant--handle-wait (fsm)
  "Handle wait-again."
  (let ((info (gptel-fsm-info fsm)))
    (when (gptel-openai-assistant--backend-is-oaia-p info)
      (unless gptel-openai-assistant-thread-id
        (user-error "No thread in current buffer to add messages!"))
      (plist-put info :openai-assistant-wait nil)
      (setf (gptel-backend-url (plist-get info :backend))
            (if (plist-get info :openai-assistant-function-call)
                (progn
                  (plist-put info :openai-assistant-function-call nil)
                  (format "https://api.openai.com/v1/threads/%s/runs/%s/submit_tool_outputs"
                        gptel-openai-assistant-thread-id
                        (plist-get info :openai-assistant-run-id)))
              (if (plist-get info :stream)
                  (format "https://api.openai.com/v1/threads/%s/runs" gptel-openai-assistant-thread-id)
                ;; FIXME: gptel-always sends POST. Ideally, this should be the GET endpoint
                ;; So using the endpoint for updating messages to get messages related to a run.
                (format "https://api.openai.com/v1/threads/%s/messages/%s"
                        gptel-openai-assistant-thread-id
                        (plist-get info :openai-assistant-message-id))))))))

(defun gptel-openai-assistant--handle-tool-use (fsm)
  "Make sure the submit-tool-outputs get triggered next."
  (plist-put (gptel-fsm-info fsm) :openai-assistant-function-call t))

(defun gptel-openai-assistant--handle-delay (fsm)
  "Handle the delay state."
  (let ((info (gptel-fsm-info fsm)))
    (and (gptel-openai-assistant--backend-is-oaia-p info)
         (let ((delay (plist-get info :openai-assistant-delay)))
           (cl-assert (numberp delay))
           (plist-put info :openai-assistant-delay nil)
           (run-at-time delay nil (lambda () (gptel--fsm-transition fsm)))))))


;; INIT should transition to AWAIT
(unless (map-nested-elt gptel-request--transitions '(INIT gptel-openai-assistant--init-to-await))
  (push '(gptel-openai-assistant--init-to-await . AWAIT) (alist-get 'INIT gptel-request--transitions)))

;; Adding TYPE -> WAIT/DELAY/AWAIT
;; DELAY & AWAIT has to happen at the veru beginning
;; WAIT should be tested right before DONE
(unless (length=
         (cl-intersection (map-keys (alist-get 'TYPE gptel-request--transitions))
                          '(gptel-openai-assistant--delay-p
                            gptel-openai-assistant--await-p
                            gptel-openai-assistant--wait-again-p))
         3)
  (setf (alist-get 'TYPE gptel-request--transitions)
        (append 
         '((gptel-openai-assistant--delay-p . DELAY)
           (gptel-openai-assistant--await-p . AWAIT))
         ;; *sigh*
         (cl-loop for transition in (alist-get 'TYPE gptel-request--transitions)
                  ;; Insert the wait again, right before DONE
                  if (eq (cdr transition) 'DONE)
                  collect '(gptel-openai-assistant--wait-again-p . WAIT) into retval
                  collect transition into retval
                  finally return retval))))

;; From DELAY go to what was expected
;; Same as TYPE but without the DELAY itself!
(unless (length=
         (cl-intersection (map-keys (alist-get 'DELAY gptel-request--transitions))
                          '(gptel-openai-assistant--await-p
                            gptel-openai-assistant--wait-again-p))
         2)
  (setf (alist-get 'DELAY gptel-request--transitions) (cl-loop for transition in (alist-get 'TYPE gptel-request--transitions)
                                                               unless (eq (cdr transition) 'DELAY)
                                                               collect transition)))

;; From AWAIT go to what was expected. Same as TYPE
(unless (alist-get 'AWAIT gptel-request--transitions)
  (setf (alist-get 'AWAIT gptel-request--transitions) (alist-get 'TYPE gptel-request--transitions)))

;; AWAIT should be tested for first in WAIT and TOOL
(unless (map-nested-elt gptel-request--transitions '(WAIT gptel-openai-assistant--await-p))
  (push '(gptel-openai-assistant--await-p . AWAIT) (alist-get 'WAIT gptel-request--transitions)))

(unless (map-nested-elt gptel-request--transitions '(TOOL gptel-openai-assistant--await-p))
  (push '(gptel-openai-assistant--await-p . AWAIT) (alist-get 'TOOL gptel-request--transitions)))

;; Adding DELAY handler
(unless (alist-get 'DELAY gptel-send--handlers)
  (push '(DELAY gptel-openai-assistant--handle-delay) gptel-send--handlers))

;; Handle the wait-again in WAIT
(unless (memq 'gptel-openai-assistant--handle-wait (alist-get 'WAIT gptel-send--handlers))
  (push 'gptel-openai-assistant--handle-wait (alist-get 'WAIT gptel-send--handlers)))

;; Handle AWAIT
(unless (alist-get 'AWAIT gptel-send--handlers)
  (push '(AWAIT gptel-openai-assistant--handle-await) gptel-send--handlers))

;; Handler for TOOL use
(unless (memq 'gptel-openai-assistant--handle-tool-use (alist-get 'TOOL gptel-send--handlers))
  (push 'gptel-openai-assistant--handle-tool-use (alist-get 'TOOL gptel-send--handlers)))

(provide 'gptel-openai-assistant)
