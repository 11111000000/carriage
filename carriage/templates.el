;;; carriage/templates.el --- Project-level templates for Carriage tests/demos

;; This file is loaded by carriage-templates-refresh (project layer) when present
;; at ROOT/carriage/templates.el and should define `carriage-templates-project'.

(defvar carriage-templates-project
  (list
   ;; Override a built-in to exercise project > user > built-in precedence.
   (list :id 'task/default
         :version "9.9"
         :label "Project Default (override)"
         :desc "Project-level override for tests and demos"
         :category 'analysis
         :render
         (concat
          "#+title: {{title|trim}} — Project Override\n\n"
          "* Goal\n- {{title}}\n\n"
          "* Plan (3–5 steps)\n1. \n2. \n3. \n\n"
          "* Context\n"
          "# Note: begin_context is inserted by the branching command.\n")))
  "Project-level Carriage templates registry (overrides built-in/user by :id).")

(provide 'carriage-templates-project)
;;; carriage/templates.el ends here
