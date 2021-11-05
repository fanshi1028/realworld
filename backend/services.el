;;; services.el --- just some service for realworld-haskell -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Francis Chan
;;
;; Author: Francis Chan <https://github.com/fanshi>
;; Maintainer: Francis Chan <jackychany321@gmail.com>
;; Created: November 02, 2021
;; Modified: November 02, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/fanshi/services
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(prodigy-define-service
  :name "realworld-haskell-postgres"
  :command "postgres"
  :cwd default-directory
  ;; :project "/users/fanshi/personal/realworld-haskell/"
  :env '(("PGDATA" "./postgres/data"))
  )

(setq lsp-sqls-connections '(((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=fanshi dbname=realworld-haskell sslmode=disable"))))
;;; services.el ends here
