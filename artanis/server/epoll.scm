;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis server epoll)
  #:use-module (artanis utils)
  #:use-module (system foreign))

(define-public EPOLL_CLOEXEC 2000000)
(define-public EPOLL_NONBLOCK 4000)

(define-public EPOLLIN #x001)
(define-public EPOLLPRI #x002)
(define-public EPOLLOUT #x004)
(define-public EPOLLRDNORM #x040)
(define-public EPOLLRDBAND #x080)
(define-public EPOLLWRNORM #x100)
(define-public EPOLLWRBAND #x200)
(define-public EPOLLMSG #x400)
(define-public EPOLLERR #x008)
(define-public EPOLLHUP #x010)
(define-public EPOLLRDHUP #x2000)
(define-public EPOLLONESHOT (expt 1 30))
(define-public EPOLLET (expt 1 31))

;; Valid opcodes ( "op" parameter ) to issue to epoll_ctl.
(define-public EPOLL_CTL_ADD 1) ; Add a file decriptor to the interface.
(define-public EPOLL_CTL_DEL 2) ; Remove a file decriptor from the interface.
(define-public EPOLL_CTL_MOD 3) ; Change file decriptor epoll_event structure.

(define-public epoll-data-meta (list '* int uint32 uint64))
(define *default-epoll-data* (list %null-pointer 0 0 0))
(define-public epoll-data-size (c/struct-sizeof epoll-data-meta))
(define-public (make-epoll-data)
  (make-c-struct epoll-data-meta *default-epoll-data*))

(define-public (epoll-data-ptr ed) (car ed))
(define-public (epoll-data-ptr-set! ed ptr)
  (list-set! (cadr ed) 0 ptr))
(define-public (epoll-data-fd ed) (cadr ed))
(define-public (epoll-data-fd-set! ed fd)
  (list-set! (cadr ed) 1 fd))
(define-public (epoll-data-u32 ed) (caddr ed))
(define-public (epoll-data-u32-set! ed u32)
  (list-set! (cadr ed) 2 u32))
(define-public (epoll-data-u64 ed) (cadddr ed))
(define-public (epoll-data-u64-set! ed u64)
  (list-set! (cadr ed) 3 u64))

(define-public epoll-event-meta (list uint32 epoll-data-meta))
(define *default-epoll-event* (list 0 *default-epoll-data*))
(define-public (make-epoll-event)
  (make-c-struct epoll-event-meta *default-epoll-event*))
(define (parse-epoll-event e)
  (parse-c-struct epoll-event-meta e))

(define-public (epoll-event-events ee) (car ee))
(define-public (epoll-event-events-set! ee e)
  (list-set! ee 0 e))
(define-public (epoll-event-data ee) (cadr ee))
(define-public (epoll-event-data-set! ee data)
  (list-set! ee 1 data))

;; Creates an epoll instance.  Returns an fd for the new instance.
;; The "size" parameter is a hint specifying the number of file
;; descriptors to be associated with the new instance.  The fd
;; returned by `epoll-create' should be closed with `close'.
(define %epoll-create
  (pointer->procedure int
                      (dynamic-func "epoll_create" (dynamic-link))
                      (list int)))

(define-public (epoll-create size)
  (let* ((efd (%epoll-create size))
         (err (errno)))
    (cond
     ((>= efd 0) efd)
     (else
      (throw 'system-error "epoll-create" "~S: ~A"
             (list size (strerror err))
             (list err))))))

;; Same as epoll_create but with an FLAGS parameter.  The unused SIZE
;; parameter has been dropped.
(define %epoll-create1
  (pointer->procedure int
                      (dynamic-func "epoll_create1" (dynamic-link))
                      (list int)))

(define-public (epoll-create1 flag)
  (let* ((efd (%epoll-create1 flag))
         (err (errno)))
    (cond
     ((>= efd 0) efd)
     (else
      (throw 'system-error "epoll-create1" "~S: ~A"
             (list flag (strerror err))
             (list err))))))

;; Manipulate an epoll instance "epfd". Returns 0 in case of success,
;; -1 in case of error ( the "errno" variable will contain the
;; specific error code ) The "op" parameter is one of the EPOLL_CTL_*
;; constants defined above. The "fd" parameter is the target of the
;; operation. The "event" parameter describes which events the caller
;; is interested in and any associated user data.
(define %epoll-ctl
  (pointer->procedure int
                      (dynamic-func "epoll_ctl" (dynamic-link))
                      (list int int int '*)))

(define-public (epoll-ctl epfd op fd event)
  (let* ((ret (%epoll-ctl epfd op fd event))
         (err (errno)))
    (cond
     ((zero? ret) ret)
     (else
      (throw 'system-error "epoll-ctl" "~S: ~A"
             (list epfd op fd (parse-epoll-event event) (strerror err))
             (list err))))))

;; Wait for events on an epoll instance "epfd". Returns the number of
;; triggered events returned in "events" buffer. Or -1 in case of
;; error with the "errno" variable set to the specific error code. The
;; "events" parameter is a buffer that will contain triggered
;; events. The "maxevents" is the maximum number of events to be
;; returned ( usually size of "events" ). The "timeout" parameter
;; specifies the maximum wait time in milliseconds (-1 == infinite).
(define %epoll-wait
  (pointer->procedure int
                      (dynamic-func "epoll_wait" (dynamic-link))
                      (list int '* int int)))

(define-public (epoll-wait epfd events maxevents timeout)
  (let* ((ret (%epoll-wait epfd events maxevents timeout))
         (err (errno)))
    (cond
     ((>= ret 0) ret)
     (else
      (throw 'system-error "epoll-wait" "~S: ~A"
             (list epfd (parse-epoll-event event) maxevents timeout(strerror err))
             (list err))))))

;; Same as epoll_wait, but the thread's signal mask is temporarily
;; and atomically replaced with the one provided as parameter.
(define %epoll-pwait
  (pointer->procedure int
                      (dynamic-func "epoll_pwait" (dynamic-link))
                      (list int '* int int '*)))

(define-public (epoll-pwait epfd events maxevents timeout sigmask)
  (let* ((ret (%epoll-pwait epfd events maxevents timeout sigmask))
         (err (errno)))
    (cond
     ((>= ret 0) ret)
     (else
      (throw 'system-error "epoll-pwait" "~S: ~A"
             (list epfd (parse-epoll-event event) maxevents timeout
                   sigmask (strerror err))
             (list err))))))
