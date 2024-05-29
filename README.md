# Chrome / Firefox plugin with Elm implementation showcase

**Attention: This plugin could lead to account restrictions if used on facebook.com. It's intended as a starting point for Chrome/Firefox plugin implementations using the Elm programming language.**

## Features
* determines the language of the \<html\> tag of the page the plugin is running in
* determines the URL of the page the plugin is running in
* has a generic state machine that executes a sequence of actions:
  1. Click on an element with a certain selector
  2. Click on an element with a certain text
  3. Click on an element inside another element
  4. Retrieve text from an element
  5. Store a retrieved value
  6. Erase a stored value
  7. Wait for an element to appear
  8. Wait for a certain URL (pattern)
  9. Fill text into an element
  10. Fill text into a contenteditable element
  11. Focus an element
  12. Conditionally skip actions (goto in state machine)

## Files

* **src/Main.elm** Main Elm program logic. Makes use of the state machine to execute a sequence of actions on the current website
* **src/Ports.elm** Interface to the JS part. Sends and receives messages.
* **src/StateMachine.elm** Executes actions on behalf of the user.
* **background.js** Forwards messages from Elm, see also **elm-import.js**
* **content.js** JS Implementation of the actions triggered by **StateMachine.elm**
* **elm-import.js** Creates the Elm program, passes Flags, interfaces Elm Port messages with **background.js** / **content.js**
* **elm.json** Elm project file
* **manifest.json** Plugin manifest file. In **firefox/manifest.json** is a slightly different version for Firefox plugins
* **popup.html** The plugin UI, references the compiled Elm program
* **publish.sh** Prepares the plugin for distribution in **dist/chrome** and **dist/firefox**. For Firefox additionally **s2f-source.zip** is created for source distribution.
