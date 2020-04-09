;; biomodels.el --- API for biomodels
;;; Commentary:
;;; Emacs interface to search the Biomodels database by description
;;; TODO If the model has already been downloaded, fontify the row
;;; TODO Add processing steps to unzip downloaded file
;;; IDEA Add new action to pop up information about model in a *model info* buffer
;;; IDEA Instead of the search/download API, use the model/download API to get the xpp file

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


(define-transient-command biomodels-transient ()
  "Test Transient Title"
  ["Actions"
   ("d" "download" biomodels-download-model)])

(defvar biomodels-download-folder "~/Documents/models/"
  "Location to download and extract the model files.")

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
                                            ".zip"))))))))
(defvar biomodels-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'biomodels-transient)
    map)
  "Keymap for biomodels-mode.")

(define-derived-mode biomodels-mode tabulated-list-mode "Biomodels"
  "major mode for displaying biomodels"
  :keymap   (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'biomodels-transient)
    map))
  ;(use-local-map biomodels-mode-map))

(provide 'biomodels-mode)
;;; biomodels.el ends here
