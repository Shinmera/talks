(chunk :name chunk-7408 :location (0.0 0.0) :size (40.0 26.0) :tile-data (kandria debug) :pixel-data "CHUNK-7408.raw" :layers ("CHUNK-7408-0.raw" "CHUNK-7408-1.raw" "CHUNK-7408-2.raw" "CHUNK-7408-3.raw" "CHUNK-7408-4.raw" "CHUNK-7408-5.raw") :background debug :bg-overlay (kandria placeholder t) :gi none :environment nil :visible-on-map-p t)
(background)
(player :location (8866.809 143.0) :name player)
(teleport-trigger :location (24.0 -24.0) :bsize (8.0 8.0) :target (840.0 40.0) :target-bsize (8.0 8.0))
(chunk :name chunk-8125 :location (640.0 0.0) :size (40.0 26.0) :tile-data (kandria debug) :pixel-data "CHUNK-8125.raw" :layers ("CHUNK-8125-0.raw" "CHUNK-8125-1.raw" "CHUNK-8125-2.raw" "CHUNK-8125-3.raw" "CHUNK-8125-4.raw" "CHUNK-8125-5.raw") :background desert :bg-overlay (kandria placeholder t) :gi none :environment nil :visible-on-map-p t)
(chunk :name chunk-8222 :location (1616.0 184.0) :size (82.0 49.0) :tile-data (kandria camp) :pixel-data "CHUNK-8222.raw" :layers ("CHUNK-8222-0.raw" "CHUNK-8222-1.raw" "CHUNK-8222-2.raw" "CHUNK-8222-3.raw" "CHUNK-8222-4.raw" "CHUNK-8222-5.raw") :background desert :bg-overlay (kandria placeholder t) :gi desert :environment desert/surface :visible-on-map-p t)
(text :name nil :active-p t :location (0.0 -32.0) :bsize (304.0 48.0) :value "KANDRIA: A Game in Common Lisp" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name a-text :active-p t :location (0.0 -104.0) :bsize (240.0 56.0) :value "shirakumo.org

@shinmera@tymoon.eu

Mastodon  Email  Cohost" :font-size 30 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name nil :active-p t :location (640.0 16.0) :bsize (304.0 96.0) :value "What is Kandria" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name b-text :active-p t :location (640.0 -56.0) :bsize (240.0 104.0) :value "• Single-player game for PC

• Released to Steam, itch.io, website on January 11th 2023

• Action RPG, Platformer

• Fully written in Common Lisp" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(interactable-sprite :location (0.0 88.0) :texture (kandria logo t) :size (1008.0 240.0) :bsize (224.0 64.0) :offset (0.0 16.0) :layer 5 :name interactable-8998)
(spawner :name spawner-9131 :location (1808.0 -80.0) :bsize (152.0 40.0) :spawn-type zombie :spawn-count 5 :spawn-args nil :active-p t :jitter-y-p t :auto-deactivate nil)
(chunk :name chunk-9154 :location (2592.0 272.0) :size (40.0 26.0) :tile-data (kandria region1) :pixel-data "CHUNK-9154.raw" :layers ("CHUNK-9154-0.raw" "CHUNK-9154-1.raw" "CHUNK-9154-2.raw" "CHUNK-9154-3.raw" "CHUNK-9154-4.raw" "CHUNK-9154-5.raw") :background caves :bg-overlay (kandria placeholder t) :gi medium :environment region1/cave :visible-on-map-p t)
(text :name d-text :active-p t :location (3216.0 216.0) :bsize (240.0 104.0) :value "• GLFW for OS window / OpenGL context

• Loading stuff during gameplay can lag out!

• Uploading data to the GPU is slow

• Need to pre-load as much as possible

• Special systems in Trial to handle preloading

• Custom shader systems (see prior publications)" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(text :name nil :active-p t :location (3232.0 248.0) :bsize (304.0 136.0) :value "Drawing" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name nil :active-p t :location (2592.0 288.0) :bsize (304.0 96.0) :value "What do we need to do?" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name c-text :active-p t :location (2592.0 216.0) :bsize (240.0 104.0) :value "• Draw things to the screen (quickly)

• Put sound to the speakers (more quickly)

• Get input from the user (quickly)

• Simulate the game (rather quickly)

• Show user interfaces

• Release the game (slowly)" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9186 :location (3232.0 272.0) :size (40.0 26.0) :tile-data (kandria region1) :pixel-data "CHUNK-9186.raw" :layers ("CHUNK-9186-0.raw" "CHUNK-9186-1.raw" "CHUNK-9186-2.raw" "CHUNK-9186-3.raw" "CHUNK-9186-4.raw" "CHUNK-9186-5.raw") :background caves :bg-overlay (kandria placeholder t) :gi medium :environment region1/cave :visible-on-map-p t)
(text :name m-text :active-p t :location (8992.0 216.0) :bsize (240.0 104.0) :value "Come hang out in #shirakumo on Libera!

Support us: https://kandria.com" :font-size 30 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name nil :active-p t :location (8992.0 248.0) :bsize (304.0 136.0) :value "Thanks!" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9207 :location (3872.0 272.0) :size (40.0 26.0) :tile-data (kandria region2) :pixel-data "CHUNK-9207.raw" :layers ("CHUNK-9207-0.raw" "CHUNK-9207-1.raw" "CHUNK-9207-2.raw" "CHUNK-9207-3.raw" "CHUNK-9207-4.raw" "CHUNK-9207-5.raw") :background mushrooms :bg-overlay (kandria placeholder t) :gi mushrooms :environment region2/hall :visible-on-map-p t)
(text :name nil :active-p t :location (3872.0 248.0) :bsize (304.0 136.0) :value "Sound" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name e-text :active-p t :location (3872.0 216.0) :bsize (240.0 104.0) :value "• Don't want to deal with huge C++ things like Wise or FMOD

• libmixed/cl-mixed/harmony: New Lisp audio stack

• GC can stall the sound thread and lead to crackles

• Large buffers can mitigate this, but lead to latency

• Ideally we could run the sound thread without GC" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9209 :location (4512.0 272.0) :size (40.0 26.0) :tile-data (kandria region2) :pixel-data "CHUNK-9209.raw" :layers ("CHUNK-9209-0.raw" "CHUNK-9209-1.raw" "CHUNK-9209-2.raw" "CHUNK-9209-3.raw" "CHUNK-9209-4.raw" "CHUNK-9209-5.raw") :background mushrooms :bg-overlay (kandria placeholder t) :gi mushrooms :environment nil :visible-on-map-p t)
(water :location (4680.0 152.0) :bsize (344.0 56.0) :fishing-spot nil)
(text :name nil :active-p t :location (4512.0 248.0) :bsize (304.0 136.0) :value "Input" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name f-text :active-p t :location (4512.0 216.0) :bsize (240.0 104.0) :value "• GLFW takes care of keyboard & mouse input

• cl-gamepad: Gamepad / Joystick / etc input devices

• OS integration (dialog boxes, fonts, localisation)

• Allow user to remap keys and buttons" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9214 :location (5152.0 272.0) :size (40.0 26.0) :tile-data (kandria region2) :pixel-data "CHUNK-9214.raw" :layers ("CHUNK-9214-0.raw" "CHUNK-9214-1.raw" "CHUNK-9214-2.raw" "CHUNK-9214-3.raw" "CHUNK-9214-4.raw" "CHUNK-9214-5.raw") :background mushrooms :bg-overlay (kandria placeholder t) :gi mushrooms :environment nil :visible-on-map-p t)
(text :name nil :active-p t :location (5152.0 248.0) :bsize (304.0 136.0) :value "Simulation" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name g-text :active-p t :location (5152.0 216.0) :bsize (240.0 104.0) :value "• Collision detection is hard to get right

• Spatial query acceleration needed

• CLOS is great to model interactions

• Behaviours compose nicely with mixins" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9216 :location (5792.0 272.0) :size (40.0 26.0) :tile-data (kandria region3) :pixel-data "CHUNK-9216.raw" :layers ("CHUNK-9216-0.raw" "CHUNK-9216-1.raw" "CHUNK-9216-2.raw" "CHUNK-9216-3.raw" "CHUNK-9216-4.raw" "CHUNK-9216-5.raw") :background mines :bg-overlay (kandria placeholder t) :gi lava-cave :environment region3/magma :visible-on-map-p t)
(magma :location (5568.0 112.0) :bsize (48.0 16.0) :fishing-spot nil)
(text :name nil :active-p t :location (5792.0 248.0) :bsize (304.0 136.0) :value "User Interfaces" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name h-text :active-p t :location (5792.0 216.0) :bsize (240.0 104.0) :value "• Can't use CLIM: no OpenGL or styling support

• Can't use Qt: no modern bindings, huge C++ blob

• Can't use GTK: even worse bindings support

• Can't use LTK: not capable enough

• Can't use CLOG: not running a browser" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9218 :location (6432.0 272.0) :size (40.0 26.0) :tile-data (kandria region3) :pixel-data "CHUNK-9218.raw" :layers ("CHUNK-9218-0.raw" "CHUNK-9218-1.raw" "CHUNK-9218-2.raw" "CHUNK-9218-3.raw" "CHUNK-9218-4.raw" "CHUNK-9218-5.raw") :background mines :bg-overlay (kandria placeholder t) :gi lava-cave :environment nil :visible-on-map-p t)
(text :name nil :active-p t :location (6432.0 248.0) :bsize (304.0 136.0) :value "Alloy" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name i-text :active-p t :location (6432.0 216.0) :bsize (240.0 104.0) :value "• New stack of protocols for UIs

• Many layouting algorithms implemented

• Including a constraint layout!

• Very easy to style with the \"presentations\"

• Support for non-pointer input devices

• Easy to integrate into a game" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(magma :location (6528.0 80.0) :bsize (80.0 40.0) :fishing-spot nil)
(chunk :name chunk-9220 :location (7072.0 272.0) :size (40.0 26.0) :tile-data (kandria region3) :pixel-data "CHUNK-9220.raw" :layers ("CHUNK-9220-0.raw" "CHUNK-9220-1.raw" "CHUNK-9220-2.raw" "CHUNK-9220-3.raw" "CHUNK-9220-4.raw" "CHUNK-9220-5.raw") :background mines :bg-overlay (kandria placeholder t) :gi lava-interior :environment region3/hall :visible-on-map-p t)
(text :name nil :active-p t :location (7072.0 248.0) :bsize (304.0 136.0) :value "Deployment" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name j-text :active-p t :location (7064.0 216.0) :bsize (240.0 104.0) :value "• Automate building for Linux & Windows

• Build from Linux with WINE

• Build from Windows with WSL2

• Build from Linux with Darling (not yet)

• Manage build configurations

• Automate uploading to Steam, Itch, etc." :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9222 :location (7712.0 272.0) :size (40.0 26.0) :tile-data (kandria region2) :pixel-data "CHUNK-9222.raw" :layers ("CHUNK-9222-0.raw" "CHUNK-9222-1.raw" "CHUNK-9222-2.raw" "CHUNK-9222-3.raw" "CHUNK-9222-4.raw" "CHUNK-9222-5.raw") :background mushrooms :bg-overlay (kandria placeholder t) :gi medium :environment region2/camp :visible-on-map-p t)
(text :name nil :active-p t :location (7712.0 248.0) :bsize (304.0 136.0) :value "User Feedback" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name k-text :active-p t :location (7704.0 216.0) :bsize (240.0 104.0) :value "• Automatically gather crash reports

• Information from custom user feedback

• Aggregated into \"Feedback\" system

• Sends savestate, screenshot, log, system info" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9224 :location (8352.0 272.0) :size (40.0 26.0) :tile-data (kandria region1) :pixel-data "CHUNK-9224.raw" :layers ("CHUNK-9224-0.raw" "CHUNK-9224-1.raw" "CHUNK-9224-2.raw" "CHUNK-9224-3.raw" "CHUNK-9224-4.raw" "CHUNK-9224-5.raw") :background caves :bg-overlay (kandria placeholder t) :gi medium :environment region1/hall :visible-on-map-p t)
(text :name nil :active-p t :location (8352.0 248.0) :bsize (304.0 136.0) :value "What's next?" :font-size 60 :h-align :center :v-align :top :offset (0.0 0.0))
(text :name l-text :active-p t :location (8352.0 216.0) :bsize (240.0 104.0) :value "• Dedicated modding support for Kandria

• Potential Nintendo Switch port

• Better Trial 3D support in the works

• Lots and lots of work to do" :font-size 30 :h-align :left :v-align :top :offset (0.0 0.0))
(chunk :name chunk-9226 :location (8992.0 272.0) :size (40.0 26.0) :tile-data (kandria camp) :pixel-data "CHUNK-9226.raw" :layers ("CHUNK-9226-0.raw" "CHUNK-9226-1.raw" "CHUNK-9226-2.raw" "CHUNK-9226-3.raw" "CHUNK-9226-4.raw" "CHUNK-9226-5.raw") :background desert :bg-overlay (kandria placeholder t) :gi desert :environment camp/camp :visible-on-map-p t)
(flag :name nil :location (9220.0 168.0) :sprite-data (kandria flag) :bsize (12.0 16.0) :layer-index 2)
