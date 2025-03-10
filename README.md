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

Right now the creation of the assistant and any related resources (i.e., vector stores, tools, etc.) needs to be done else where (like on the dashboard or playground). Changing the model in `gptel-menu` would update the model used by the assistant.

## Handling tool-calls (function calls)
TODO

## Handling file annotations
The file annotations will apear as `[file_citation:<file-id>]` in the text, where `<file-id>` the [file-id](https://platform.openai.com/docs/api-reference/files/object#files/object-id) of the corresponding file. To replace it the `gptel-post-response-functions` abnormal hook can be used. gptel-openai-assistant comes with `gptel-openai-assistant-replace-annotations-with-filename`, which is a function that can be used with `gptel-post-response-functions`. It repalces the `file_citations` with the filenames by querying the api. This will replace `[file_citation:<file-id>]` with  `[file:<filename>]`.
