@(heading "sicp-streams")

(module sicp-streams
  @("Stream procedures from section 3.5")
  (cons-stream
   display-line
   display-stream
   scale-stream
   stream-car
   stream-cdr
   stream-enumerate-interval
   stream-filter
   stream-for-each
   stream->list
   stream-map
   stream-null
   stream-null?
   stream-ref
   the-empty-stream)
  (import chicken scheme)
  (import-for-syntax matchable)
  (include "sicp-streams-core.scm"))
