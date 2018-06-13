/**
 * @author Andrew Jarombek
 * @since 6/12/2018
 * Web worker code to respond to messages from the main thread
 */

// Event handler for the web worker to respond to messages
onmessage = (e) => {

    console.info(`[WORKER] Received Note: ${JSON.stringify(e.data)}`);

    // Add a timestamp to the note and pass it back to the main thread
    const date = new Date();
    const result = {note: `${e.data.note} - ${date.toString()}`};

    console.info(`[WORKER] Sending Back Note: ${JSON.stringify(result)}`);
    postMessage(result);
};