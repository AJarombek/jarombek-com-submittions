/**
 * @author Andrew Jarombek
 * @since 6/12/2018
 * DOM manipulation and web worker initialization + message passing for the
 * web workers demo
 */

const noteInput = document.getElementById('noteInput');
const outputDiv = document.querySelector('.output');

// Worker will be in the window object if this browser supports web workers
if (window.Worker) {

    // Create a new web worker instance with the local file worker.js
    const noteWorker = new Worker("worker.js");

    noteInput.addEventListener("keyup", (event) => {
        event.preventDefault();

        // Check if the enter button was pressed
        if (event.keyCode === 13) {
            const newNote = noteInput.value;
            noteInput.value = "";

            console.info("'Enter' Clicked!");

            // If so pass the input elements value to the web worker for processing
            noteWorker.postMessage({note: newNote});
            console.info(`[CLIENT] Note Passed to Worker: ${newNote}`);
        }
    });

    // Listen for the onmessage event from the web worker
    noteWorker.onmessage = (e) => {

        console.info(`[CLIENT] Note Received from Worker: ${JSON.stringify(e.data)}`);

        // When a message is received from the web worker a new HTML element will be
        // created displaying the workers message
        const newNote = document.createElement("div");
        const noteContent = document.createTextNode(e.data.note);
        newNote.appendChild(noteContent);
        outputDiv.prepend(newNote);
    }

} else {
    console.error("[CLIENT] Web Worker Not Supported!");
    console.error(window);
}