const noteInput = document.querySelector('#noteInput');
const outputDiv = document.querySelector('.output');

if (window.Worker) {

    const noteWorker = new Worker("worker.js");

    noteInput.addEventListener("keyup", (event) => {
        event.preventDefault();

        // Check if the enter button was pressed
        if (event.keyCode === 13) {
            console.info("'Enter' Clicked!");

            noteWorker.postMessage({note: noteInput.value});
            console.info(`[CLIENT] Note Passed to Worker: ${noteInput.value}`);
        }
    });

    noteWorker.onmessage = (e) => {
        const newNote = document.createElement("div");
        const noteContent = document.createTextNode(e.data.note);
        newNote.appendChild(noteContent);
        outputDiv.appendChild(noteContent);
        console.info(`[CLIENT] Note Received from Worker: ${JSON.stringify(e.data)}`);
    }

} else {
    console.error("[CLIENT] Web Worker Not Supported!");
    console.error(window);
}