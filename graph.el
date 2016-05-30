;;; graph.el --- Draw directed acyclic graphs in ascii

;;; Commentary:
;;
;; Gansner, Emden R., et al. "A technique for drawing directed graphs."
;; Software Engineering, IEEE Transactions on 19.3 (1993): 214-230.

;;; Code:
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl))

(cl-defstruct node label edges visisted)
(cl-defstruct edge tail head)
(cl-defstruct graph nodes)

(define-error 'graph-error "Something in the graph went wrong")
(define-error 'graph-value-error "Value given is wrong")
(define-error 'node-not-in-graph "Node is not part of Graph" 'graph-error)

(defun init-graph ()
  "Create a graph with a root node."
  (make-graph :nodes (list (make-node :label "root"))))

(defun root (graph)
  "Return the root of GRAPH."
  (car (graph-nodes graph)))

(defun add-node (node graph &optional head)
  "Connect a new NODE to HEAD node of GRAPH.
If HEAD is nil, use the root node of GRAPH."
  (let* ((nodes (graph-nodes graph))
         (head (or head (root graph))))
    (unless (member head nodes)
      (signal 'node-not-in-graph (list head graph)))
    (setf (graph-nodes graph) (add-to-list 'nodes node t))
    (add-edge node head graph)))

(defun add-edge (from to graph)
  "Add edge FROM node TO node in GRAPH."
  (let ((edge (make-edge :tail from :head to))
        (edges (node-edges from)))
    (setf (node-edges from) (add-to-list 'edges edge t))))

(defun get-node (label nodes)
  "Return node that has LABEL from NODES."
  (when nodes
    (cl-find label nodes :key 'node-label :test 'equal)))

(defun can-add-node (label dependencies graph)
  "Return t if all dependencies are in graph."
  (let ((nodes (graph-nodes graph)))
    (if dependencies
        (when (= (length dependencies)
                 (length (remove nil (mapcar (lambda (dep) (get-node dep nodes)) dependencies))))
          t)
      t)))

(defun create-dependency-graph (nodealist)
  "Create a graph from a alist of labels with dependencies.

(list (list \"a\" \"b\" \"c\") (list \"b\") (list \"c\")) will create a dependency graph:

root -- b -- a
    \\-- c --/
"
  (let ((g (init-graph))
        (nodealist (sort nodealist (lambda (a b) (< (length a) (length b)))))) ; nodes with less dependencies first
    (while nodealist
      (mapc
       (lambda (labeldependencies)
         (let* ((label (car labeldependencies))
                (dependencies (cdr labeldependencies))
                (node (make-node :label label))
                (nodes (graph-nodes g)))
           (when (can-add-node label dependencies g)  ;; are all dependencies already in the graph?
             (add-node node g (get-node (car dependencies) nodes))  ;; if (car dependencies) is nil use root
             (mapc
              (lambda (dependency)
                (add-edge node (get-node dependency nodes) g))
              (cdr dependencies))
             (setq nodealist (delete labeldependencies nodealist)))))
       nodealist))
    g))

(provide 'graph)
;;; graph.el ends here
