;; biomodels.el --- API for biomodels
;;; Code:

(require 'request)
(require 'json)

(defun biomodels ()
  "Search biomodels for searchterm.
This returns the top 10 model IDs from biomodels"
  (interactive)
  (setq searchterm (read-string "Search: "))
  (message "Searching for: %s" searchterm)
  (request
   "https://www.ebi.ac.uk/biomodels/search"
   :parser 'json-read
   :headers '(("accept" . "application/json"))
   :params `(("query" . ,(format "%s" searchterm))
             ("offset" . 0)
             ("numResults" . 50)
             ("sort" . "relevance-desc"))
   :type "GET"
   :success (cl-function
             (lambda (&key data response &allow-other-keys)
               (let* ((allmodels (assoc-default 'models data))
                      (ids (mapcar (lambda (x) (assoc-default 'id x)) allmodels))
                      (names (mapcar (lambda (x) (assoc-default 'name x)) allmodels))
                      (columns [("ID" 20) ("Name" 100)])
                      (rows (mapcar* (lambda (x y)  `(nil [,x,y])) ids names)))
                 ;; (message "%s" allmodels)
                 ;; (message "%s" (length names))
                 ;; (message "%s" (vectorp allmodels))
                 ;; (message "%s" (length allmodels))
                 ;; (message "%s" ids)
                 ;; (message "%s" names)
                 ;; (message "%s" rows)
                 ;; (message "%s" (request-response-url response))
                 ;; (message "%s" (assoc-default 'matches data))
                 (switch-to-buffer "*biomodels*")
                 (biomodels-mode)
                 (setq tabulated-list-format columns)
                 (setq tabulated-list-entries rows)
                 (tabulated-list-init-header)
                 (tabulated-list-print))
               ))))

(defvar biomodels-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'biomodels-transient)
   map))

(define-transient-command biomodels-transient ()
  "Test Transient Title"
  ["Actions"
   ("d" "download" biomodels-download-model)])

(defvar biomodels-download-folder "~/Documents/models/")

(defun biomodels-download-model()
  "Download model under point."
  (interactive)
  (let ((modelid (aref (tabulated-list-get-entry) 0)))
    (request
      "https://www.ebi.ac.uk/biomodels/search/download"
      :params `(("models" . ,(format "%s" modelid)))
      :encoding 'binary
      :headers '(("accept" . "application/zip"))
      :type "GET"
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((coding-system-for-write 'binary))
                    (write-region data nil (concat
                                            biomodels-download-folder
                                            modelid
                                            ".zip"))))))
    )
(define-derived-mode biomodels-mode tabulated-list-mode "biomodels"
  "major mode for displaying biomodels")
;;; biomodels.el ends here
