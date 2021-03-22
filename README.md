# emacs-attacks
Simple top-down wargame for Emacs

Why:
- .. because we needed a wargame in Emacs right? People can't complain there isn't one anymore!
- I wanted to learn Lisp and elisp in particular; failed spectacularly - this is not written in idiomatic lisp, and uses all terrible conventions. Thats what you get when hacking round at 2am .. but it works :)
- inspired by Empire - Wargame of the Century (Walter Bright, mid 1980s)

How to run:
- Clone the git or otherwise unpack to somewhere
- Open up emattacks.el
- M-x eval-buffer
- M-x emx/run-emacs-attacks

What does it do?
- as of Feb 2021, it just draws a lame map in its buffer, and a fake side-panel, and lets you cursor around
- immediate priorities are properly handlig the 'sprite list' (NPC and PC movable units, as opposed to map tiles); a few strategies for display (text properties? posframes? just replace map tile with the 'sprite' art..)
- proper side panel content and menus
- map painting, saving and loading
- hook people into working on mapscripts and AI scripts (bresenham bee line AI isn't very useful, especially with water in the way)
- large map handling (scrolling the map viewport, not actually the buffer.. rendering this many small images slows down Emacs when its a huge map!)

How to play? (to be expanded upon once there is much to do..)
- viewport - Emacs speed at rendering thousands of small images isn't great, so if your Emacs isn't performing well, or if you just prefer, you can set a smaller viewport size; the viewport is the region size of the map to render, and it can scroll around the map surface (assuming the viewport is smaller than the map.) The term viewport is chosen due to Emacs already using terms like window and frame in odd fashions.

Contributing:
- all are welcome!
- check the license - currently aiming for GPLv3 on all assets for maximum compatibility with Emacs itself
- we especially need the following I think:
  - Artists - I've swiped some tiles from Freeciv for now (I believe GPLv3 but didn't keep notes), so I'd like to replace the tiles with new art so we're not in question of ownership
  - elisp - Mapscripts need writing, to generate fresh maps rather than the dumb demo ones I've made
  - elisp - AI scripts for the NPC units

Emacs version:
- Tested on Emacs 27.1
- Currently requires GUI mode Emacs, until we fix/write the text interface

Required packages:
- posframe
- ..

Screenshots:

- very first alpha screenshot
![Rudimentary first screenshot](./screenshots/emattacks-alpha-001.png)

To Do:
- much is to be done, this is very rudimentary so far! Not worth listing what is to do, because far more needs to be done than is done!

Detail todo from Feb 2021:

```elisp
;; - cursor around; select single unit
;; - proper documentation directory and link from README
;; - populate side panel correctly per unit (cursor hovered unit, selected unit, tile, help keys)
;; - AI object and handlers, basic move scripts
;; - end of turn handler (iterate across other players AI scripts)
;; - unit enter unit options and combat resolver
;; - move archetype definition from el to json for easier modding
;; - re-render after key/function
;; - map paint + save mode
;; - map loading
;; - mapscript or two? or gen noise from py?
;; - buffer-local vars for it all (multiple gaems at once?), or at least multiple viewports could be very handy/neat (unit, focus? spying?)
;; - key actions .. and move game-specific key handling to the game module somewhere
;; - refactor .. code to separation of concern; and why is module defining viewport size? mapscripts hardcoded?..etc
;; - refactor to ditch the concept of multiple games; keep it simple, its all about the wargame, if we want to do pushem, do a separate package
;; - add to makefile an image generator (merge enemy0 and player bg with units to emit the assorted unit files)
;; - FARM OUT: AI scripts
;; - FARM OUT: art
;; - FARM OUT: mapscripts
;; - combat resolution
;; - additional units
;; - production queue
;; - AI register function, set state personality
;; - AI function invoke
;; - game profiles vs start emattacks -> pick module -> pick map size -> mapscript choice -> then proceed?
;;   - proper module picker
;;   - proper size picker
;;   - proper mapscript picker
;; - refactor module code to base package code for cross-module mojination
;; - refactor ... drop other game modes, all emx; but allow dirs for drop in rule overriders etc? or hooks after units, after turns etc?
;; - rectangle select a group of units? or assign units to a name, and select group by name?
```
