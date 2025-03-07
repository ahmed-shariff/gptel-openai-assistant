# gptel-openai-assistant

See https://github.com/karthink/gptel/discussions/539 for more information on how this came about.

**NOTE: This is in active development**

## Setup
```emacs-lisp
(use-package gptel-openai-assistant
  :after gptel
  :straight (gptel-openai-assistant :type git :host github :repo "ahmed-shariff/gptel-openai-assistant")
  :config
  ;; Create and add openai-assistant as a known backend
  (setf (alist-get "openai-assistant" gptel--known-backends
                   nil nil #'equal)
        (gptel-make-openai-assistant "openai-assistant" :key (gptel--get-api-key)))
  ;; The id of the assistant. Can be copied from the dashboard.
  (setf gptel-openai-assistant-assistant-id "<assistant id>"))
```

Right now the creation of the assistant and any related resources (i.e., vector stores, tools, etc.) needs to be done else where (like on the dashboard). Note that setting the model on gptel won't make a difference for the the assistant, it will use whaever model configured on the assistant.
