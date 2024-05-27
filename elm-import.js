document.addEventListener("DOMContentLoaded", function () {
    var div = document.getElementById("elm-main");

    chrome.storage.local.get(['streamKey', 'serverUrl', 'liveStreamingPageUrl'], function(items) {
        const app = Elm.Main.init({
            node: div,
            flags: {
                url: window.location.href,
                title: document.title,
                streamKey: items.streamKey || '',
                serverUrl: items.serverUrl || '',
                liveStreamingPageUrl: items.liveStreamingPageUrl || ''
            }
        });

        // Function to send messages to the content script
        app.ports.sendMessageToContentScript.subscribe((message) => {
            chrome.runtime.sendMessage(message, (response) => {
                if (response) {
                    app.ports.receiveMessageFromContentScript.send(response);
                } else if (chrome.runtime.lastError != null) {
                    console.error('Error receiving response:', chrome.runtime.lastError);
                }
            });
        });
    });
});
