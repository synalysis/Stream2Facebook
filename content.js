chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    console.log('Received message:', message);

    if (message.type === 'processingFinished') {
        console.log('Processing finished');
        sendResponse({ type: 'PROCESSING_COMPLETED'});
    } else if (message.type === 'click') {
        const element = document.querySelector(message.selector);
        if (element) {
            element.click();
            console.log('Click action completed:', message.description);
            sendResponse({ type: 'ACTION_COMPLETED' });
        } else {
            console.error('Click action failed:', message.description);
            sendResponse({ type: 'ACTION_FAILED', description: message.description });
        }
    } else if (message.type === 'retrieve') {
        const elements = document.querySelectorAll(message.selector);
        let found = false;
        let value = null;

        elements.forEach(element => {
            if (element.value !== undefined) {
                if (message.pattern) {
                    const regex = new RegExp(message.pattern);
                    if (regex.test(element.value)) {
                        value = element.value;
                        found = true;
                    }
                } else {
                    value = element.value;
                    found = true;
                }
            }

            if (found) {
                console.log('Retrieve action completed:', message.description, ' (', message.key, '/', value, ')');
                sendResponse({
                    type: 'RETRIEVE_COMPLETED',
                    key: message.key,
                    value: value
                });
                return;
            }
        });

        if (!found) {
            console.error('Retrieve action failed:', message.description);
            sendResponse({ type: 'ACTION_FAILED', description: message.description });
        }
    } else if (message.type === 'wait') {
        const { selector, timeout, retries, description, textContent } = message;
        if (textContent) {
            waitForElementWithText(selector, textContent, timeout, retries)
                .then(() => {
                    console.log('Wait action completed:', description);
                    sendResponse({ type: 'ACTION_COMPLETED' });
                })
                .catch(() => {
                    console.error('Wait action failed:', description);
                    sendResponse({ type: 'ACTION_FAILED', description: description });
                });
        } else {
            waitForElement(selector, timeout, retries)
                .then(() => {
                    console.log('Wait action completed:', description);
                    sendResponse({ type: 'ACTION_COMPLETED' });
                })
                .catch(() => {
                    console.error('Wait action failed:', description);
                    sendResponse({ type: 'ACTION_FAILED', description: description });
                });
        }
    } else if (message.type === 'waitForUrl') {
        const { urlPattern, timeout, retries, description } = message;
        waitForUrl(urlPattern, timeout, retries)
            .then(() => {
                console.log('Wait for URL action completed:', description);
                sendResponse({ type: 'ACTION_COMPLETED' });
            })
            .catch(() => {
                console.error('Wait for URL action failed:', description);
                sendResponse({ type: 'ACTION_FAILED', description: description });
            });
    } else if (message.type === 'nestedClick') {
        const parentElement = document.querySelector(message.parentSelector);
        if (parentElement) {
            const childElement = parentElement.querySelector(message.childSelector);
            if (childElement) {
                childElement.click();
                console.log('Nested click action completed:', message.description);
                sendResponse({ type: 'ACTION_COMPLETED' });
            } else {
                console.error('Nested click action failed:', message.description);
                sendResponse({ type: 'ACTION_FAILED', description: message.description });
            }
        } else {
            console.error('Nested click action failed:', message.description);
            sendResponse({ type: 'ACTION_FAILED', description: message.description });
        }
    } else if (message.type === 'clickByText') {
        const elements = document.getElementsByTagName(message.tag);
        const regex = new RegExp(message.text);
        let found = false;
        for (let element of elements) {
            if (regex.test(element.textContent.trim())) {
                element.click();
                found = true;
                console.log('Click by text action completed:', message.description);
                sendResponse({ type: 'ACTION_COMPLETED' });
                break;
            }
        }
        if (!found) {
            console.error('Click by text action failed:', message.description);
            sendResponse({ type: 'ACTION_FAILED', description: message.description });
        }
    } else if (message.type === 'fillValue') {
        const element = document.activeElement;
        if (element && (element.tagName === 'INPUT' || element.tagName === 'TEXTAREA' || (element.tagName === 'DIV' && element.role === 'textbox'))) {
            simulateTyping(element, message.value)
                .then(() => {
                    console.log('Fill value action completed');
                    sendResponse({ type: 'ACTION_COMPLETED' });
                })
                .catch((error) => {
                    console.error('Fill value action failed:', error);
                    sendResponse({ type: 'ACTION_FAILED', description: error });
                });
        } else {
            console.error('Fill value action failed: No active input, textarea, or div with role textbox element');
            sendResponse({ type: 'ACTION_FAILED', description: 'No active input, textarea, or div with role textbox element' });
        }
    } else if (message.type === 'fillChildPValue') {
        const activeElement = document.activeElement;
        if (activeElement) {
            let pElement = activeElement.querySelector('p');
            if (!pElement) {
                pElement = document.createElement('p');
                activeElement.appendChild(pElement);
            }

            // Clear the <p> element using innerHTML
            pElement.innerHTML = '';
            console.log('Children after clearing with innerHTML:', pElement.childNodes); // Debugging log

            // Set the text content for contenteditable
            simulateTypingSimple(pElement, message.value);
            console.log('pElement innerHTML after setting text:', pElement.innerHTML); // Debugging log

            console.log('Fill value in child <p> action completed:', message.value);
            sendResponse({ type: 'ACTION_COMPLETED' });
        } else {
            console.error('Fill value in child <p> action failed: No active element');
            sendResponse({ type: 'ACTION_FAILED', description: 'No active element' });
        }
    } else if (message.type === 'typeText') {
        const element = document.activeElement;
        if (element && (element.tagName === 'INPUT' || element.tagName === 'TEXTAREA' || (element.tagName === 'DIV' && element.role === 'textbox'))) {
            simulateTyping(element, message.text)
                .then(() => {
                    console.log('Type text action completed');
                    sendResponse({ type: 'ACTION_COMPLETED' });
                })
                .catch((error) => {
                    console.error('Type text action failed:', error);
                    sendResponse({ type: 'ACTION_FAILED', description: error });
                });
        } else {
            console.error('Type text action failed: No active input, textarea, or div with role textbox element');
            sendResponse({ type: 'ACTION_FAILED', description: 'No active input, textarea, or div with role textbox element' });
        }
    } else if (message.type === 'moveToNextElement') {
        const currentElement = document.activeElement;
        if (currentElement) {
            const nextElement = currentElement.nextElementSibling;
            if (nextElement && nextElement.tabIndex !== -1) {
                nextElement.focus();
                console.log('Move to next element action completed');
                sendResponse({ type: 'ACTION_COMPLETED' });
            } else {
                console.error('Move to next element action failed: No next focusable element');
                sendResponse({ type: 'ACTION_FAILED', description: 'No next focusable element' });
            }
        } else {
            console.error('Move to next element action failed: No active element');
            sendResponse({ type: 'ACTION_FAILED', description: 'No active element' });
        }
    } else if (message.type === 'focusElement') {
        const element = document.querySelector(message.selector);
        if (element) {
            element.click();
            element.focus();
            if (element.isContentEditable && element.firstChild) {
                let range = document.createRange();
                let sel = window.getSelection();
                range.setStart(element.firstChild, 0);
                range.collapse(true);
                sel.removeAllRanges();
                sel.addRange(range);
            }
            if (element.isContentEditable) {
                element.firstChild.click();
                element.innerHTML = element.innerHTML; // Forces the browser to render the cursor
            }
            console.log('Focus element action completed:', message.description);
            sendResponse({ type: 'ACTION_COMPLETED' });
        } else {
            console.error('Focus element action failed:', message.description);
            sendResponse({ type: 'ACTION_FAILED', description: message.description });
        }
    } else if (message.type === 'log') {
        console.log('Log action:', message.message);
        sendResponse({ type: 'ACTION_COMPLETED', message: message.message });
    }

    // Ensure the response is sent asynchronously
    return true;
});

function waitForElement(selector, timeout, retries) {
    return new Promise((resolve, reject) => {
        const interval = setInterval(() => {
            if (document.querySelector(selector)) {
                clearInterval(interval);
                resolve();
            } else if (retries <= 0) {
                clearInterval(interval);
                reject();
            }
            retries--;
        }, timeout);
    });
}

function waitForElementWithText(tag, textPattern, timeout, retries) {
    return new Promise((resolve, reject) => {
        const regex = new RegExp(textPattern);
        const interval = setInterval(() => {
            const elements = document.getElementsByTagName(tag);
            let found = false;
            for (let element of elements) {
                if (regex.test(element.textContent.trim())) {
                    clearInterval(interval);
                    resolve();
                    found = true;
                    break;
                }
            }
            if (!found && retries <= 0) {
                clearInterval(interval);
                reject();
            }
            retries--;
        }, timeout);
    });
}

function waitForUrl(urlPattern, timeout, retries) {
    return new Promise((resolve, reject) => {
        const regex = new RegExp(urlPattern);
        const interval = setInterval(() => {
            if (window.location.href.match(regex)) {
                clearInterval(interval);
                resolve();
            } else if (retries <= 0) {
                clearInterval(interval);
                reject();
            }
            retries--;
        }, timeout);
    });
}

function simulateTyping(element, text) {
    return new Promise((resolve) => {
        let index = 0;

        function typeCharacter() {
            if (index < text.length) {
                element.click();

                const char = text.charAt(index);
                const event = new Event('input', {
                    bubbles: true,
                    cancelable: true,
                });
                const keyDownEvent = new KeyboardEvent('keydown', { bubbles: true, cancelable: true, key: char });
                const keyPressEvent = new KeyboardEvent('keypress', { bubbles: true, cancelable: true, key: char });
                const inputEvent = new InputEvent('input', { bubbles: true, cancelable: true, data: char });
                const keyUpEvent = new KeyboardEvent('keyup', { bubbles: true, cancelable: true, key: char });

                element.dispatchEvent(keyDownEvent);
                element.dispatchEvent(keyPressEvent);
                element.value += char;
                element.dispatchEvent(inputEvent);
                element.dispatchEvent(keyUpEvent);
                // element.dispatchEvent(event);
                index++;

                element.click();
                setTimeout(typeCharacter, 5); // Adjust typing speed if needed
            } else {
                resolve();
            }
        }

        typeCharacter();
    });
}

function simulateTypingSimple(element, text) {

    document.execCommand("insertText", false, text);

    // Dispatch a change event at the end
    const inputEvent = new InputEvent('input', { bubbles: true, cancelable: true});
    element.dispatchEvent(inputEvent);
    const changeEvent = new Event('change', { bubbles: true, cancelable: true });
    element.dispatchEvent(changeEvent);
}


function simulateTab() {
    const currentElement = document.activeElement;
    const nextElement = currentElement.nextElementSibling;

    if (nextElement && nextElement.tabIndex !== -1) {
        nextElement.focus();
    } else {
        console.error('Simulate tab action failed: No next focusable element');
    }
}
