;; biomodels.el --- API for biomodels
;;; Commentary:
;;; Emacs interface to search the Biomodels database by description
;;; TODO If the model has already been downloaded, fontify the row
;;; TODO Add processing steps to unzip downloaded file
;;; DONE Add new action to pop up information about model in a *model info* buffer
;;; IDEA Instead of the search/download API, use the model/download API to get the xpp file

;;; Code:

(require 'request)
(require 'json)

(defvar biomodels-download-folder "~/.biomodels/"
  "Location to download and extract the model files.")

(defvar biomodels-attributes-alist 
  '((species . "--species")
   (reactions . "--reactions")
   (description . "--description"))
  "List of model attributes.")

(defvar biomodels-download-alist 
  '((xpp . "xpp")
    (png . "png")
    (pdf . "pdf")
    (sbml . "zip"))
  "List of file types available for download.")

(define-transient-command biomodels-download-transient ()
  "Test Transient Title"
  ["Actions"
   ("x" "XPP" biomodels-download-xpp)
   ("p" "PNG" biomodels-download-png)
   ("s" "SBML" biomodels-download-sbml)
   ("P" "PDF" biomodels-download-pdf)
   ("a" "All filetypes" biomodels-download-all-filetypes)
   ])

(defun biomodels-download-model (filetype)
  "Download model under point."
  (let ((modelid (aref (tabulated-list-get-entry) 0)))
    (message "%s" modelid)
    (if (member (concat modelid "." filetype) (biomodels--local-models))
        (message "%s already exists" modelid)
      (progn 
        (write-region (request-response-data
                       (request
                         (format "https://www.ebi.ac.uk/biomodels/search/download/%s" modelid)
                         :encoding 'binary
                         :params `(("filename" . ,(format "%s.%s" modelid filetype)))
                         :headers '(("accept" . "application/octet-stream"))
                         :sync t))
                      nil (concat biomodels-download-folder modelid "." filetype))))))

(defun biomodels-download-sbml ()
  ""
  (interactive)
  (biomodels-download-model (cdr (assoc 'sbml biomodels-download-alist))))

(defun biomodels-download-png ()
  ""
  (interactive)
  (biomodels-download-model (cdr (assoc 'png biomodels-download-alist))))

(defun biomodels-download-xpp ()
  ""
  (interactive)
  (biomodels-download-model (cdr (assoc 'xpp biomodels-download-alist))))

(defun biomodels-download-pdf ()
  ""
  (interactive)
  (biomodels-download-model (cdr (assoc pdf biomodels-download-alist))))

(defun biomodels-download-all-filetypes ()
  ""
  (interactive)
  (biomodels-download-sbml)
  (biomodels-download-png)
  (biomodels-download-xpp)
  (biomodels-download-pdf)
)


(defun biomodels-get-local-models ()
  "Make list of available files."
  (interactive)
  (let* (
         ;;(models (s-split "\n" (shell-command-to-string (concat "ls " biomodels-download-folder "*.zip"))))
         (models (biomodels--local-models))
         (ids  (map 'list 'file-name-base models))
         (names (mapcar (lambda (x) (message "%s" (concat "./sbml-interface.py --model " biomodels-download-folder x " --description"))
                          (shell-command-to-string (concat "./sbml-interface.py --model " biomodels-download-folder x ".zip --description"))) ids))
         (columns [("ID" 20) ("Name" 100)])
         (rows (mapcar* (lambda (x y)  `(nil [,x,y])) ids names)))
    (message "%s" models)
    (switch-to-buffer "*biomodels*")
    (biomodels-mode)
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print))
    )

(defun biomodels--local-models ()
  "Return list of local models."
  (directory-files biomodels-download-folder nil ".zip\\|.png\\|.pdf\\|.xpp")
)

(defun biomodels-search ()
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
                      (columns [("ID" 20)("Name" 100)])
                      (rows (mapcar* (lambda (x y)  `(nil [,x,y])) ids names)))
                 (switch-to-buffer "*biomodels*")
                 (biomodels-mode)
                 (setq tabulated-list-format columns)
                 (setq tabulated-list-entries rows)
                 (tabulated-list-init-header)
                 (tabulated-list-print))
               ))))



(defun biomodels-get-model-attribute (attype)
  "Display species in model under point."
  (let* ((modelid (aref (tabulated-list-get-entry) 0))
         (attribute (cdr (assoc attype biomodels-attributes-alist)))
        (buf (get-buffer-create (format "*biomodels -- %s %s*" modelid attribute))))
    (if (buffer-live-p buf)
        (kill-buffer buf))
    (setq buf (get-buffer-create (format "*biomodels -- %s %s*" modelid attribute)))
    (with-current-buffer buf
      (insert 
       (shell-command-to-string (concat "./sbml-interface.py --model " biomodels-download-folder modelid ".zip " attribute))))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (special-mode)
    ))

(defun biomodels-get-species ()
  ""
  (interactive)
  (biomodels-get-model-attribute 'species))

(defun biomodels-get-reactions ()
  ""
  (interactive)
  (biomodels-get-model-attribute 'reactions))

    
(defvar biomodels-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "D") 'biomodels-download-transient)
    (define-key map (kbd "s") 'biomodels-get-species)
    (define-key map (kbd "r") 'biomodels-get-reactions)
    map)
  "Keymap for biomodels-mode.")

(define-derived-mode biomodels-mode tabulated-list-mode "Biomodels"
  "major mode for displaying biomodels"
  :keymap   (let ((map (make-sparse-keymap)))
    (define-key map (kbd "D") 'biomodels-download-transient)
;;    (define-key map (kbd "d") 'biomodels-download-model)
    (define-key map (kbd "s") 'biomodels-get-species)
    (define-key map (kbd "r") 'biomodels-get-reactions)
    map))
  ;(use-local-map biomodels-mode-map))

(provide 'biomodels-mode)
;;; biomodels.el ends here
