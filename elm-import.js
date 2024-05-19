document.addEventListener("DOMContentLoaded", function () {
    var div = document.getElementById("elm-main");
    // Elm.Main.embed(div);

        const app = Elm.Main.init({
            node: div,
            flags: {
                url: window.location.href,
                title: document.title,
                lang: document.documentElement.lang || 'en'
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
