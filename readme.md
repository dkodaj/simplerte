# V simple rich text editor in Elm

Creates a contenteditable div and updates the Elm model via ports.

**Warning**: Uses execCommand, which has been [deprecated](https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand). While it lasts, you can check out the [live version](https://dkodaj.github.io/simplerte).