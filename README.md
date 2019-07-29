Serial Controlled OLED Display
==============================

Two programs in one!

1. Arduino code to read commands from a serial port and update an
   OLED display.

2. A Haskell program to display various bits of information on that
   OLED display via the serial port.  This is the controller program
   that listens for D-Bus signals and updates the display accordingly.

**NOTE:** Since the controller program uses D-Bus for interprocess
communication it's likely to only work on Linux.  Patches are welcome.

Building Everything
-------------------

This repository contains files for the [Nix package manager][nix] and
it's the way I recommend you build the two packages.

Once you have Nix installed the rest is easy:

1. Build and flash a Arduino Pro Micro:

         $ cd arduino && nix-shell --run upload

2. Build and start the controller program:

         $ cd control && nix-build
         $ ./result/bin/display-control

[nix]: https://nixos.org/nix/

Pomodoros and Org Clock
-----------------------

The following Emacs Lisp will send D-Bus signals when you clock in or
out.  The Haskell program in this package will respond to those
signals by updating the OLED display to show a Pomodoro timer.


```lisp
(defun pjones:org-clock-update-dbus ()
  "Broadcast a D-Bus signal with the latest `org-clock' data.

This exposes the current clock's start time and heading to any process
listening to the correct D-Bus signal.

You can monitor this signal via the following command:

    dbus-monitor type='signal',interface='org.gnu.Emacs.Org.Clock'

Read the code below for the two event names and the signal arguments
they provide."
  (if (org-clocking-p)
      (let ((start-time (floor (float-time org-clock-start-time)))
            (description org-clock-heading))
        (dbus-send-signal
         :session nil dbus-path-emacs
         (concat dbus-interface-emacs ".Org.Clock") "Started"
         start-time description))
    (dbus-send-signal
     :session nil dbus-path-emacs
     (concat dbus-interface-emacs ".Org.Clock") "Stopped")))

(let ((hooks '( org-clock-in-hook
                org-clock-out-hook
                org-clock-cancel-hook )))
  (dolist (hook hooks)
    (add-hook hook #'pjones:org-clock-update-dbus)))
```

Current Org Clock Heading
-------------------------

Want to display the OrgMode heading for the currently clocked-in task
somewhere?  Perhaps a status bar?

```
curl --silent --unix-socket ~/.display-control.sock 'http://localhost/message' | jq --raw-output
```
