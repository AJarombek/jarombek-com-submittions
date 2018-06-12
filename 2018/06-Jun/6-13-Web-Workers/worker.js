onmessage = (e) => {

    console.info(`[WORKER] Received Note: ${JSON.stringify(e.data)}`);

    const date = new Date();
    const result = {note: `${e.data.note} - ${date.toString()}`};

    console.info(`[WORKER] Sending Back Note: ${JSON.stringify(result)}`);
    postMessage(result);
};