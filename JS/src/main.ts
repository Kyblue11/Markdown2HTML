import { from, fromEvent, merge, of } from "rxjs";
import { map, mergeScan, first, mergeWith, mergeMap } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    mergeWith(of(markdownInput.value)),
    // append the file input upload to the markdown input
    map((value) => (s) => ({
        ...s,
        markdown: s.initialHTML + "\n" + value,
        helpVisible: false,
    })),
);

// Fixed: The toggle Render converted from a Checkbox to a Button
const toggleRenderButton = document.getElementById(
    "toggle-render-button",
) as HTMLButtonElement;
const toggleRenderStream$: Observable<Action> = fromEvent(
    toggleRenderButton,
    "click",
).pipe(map(() => (s: State) => ({ ...s, renderHTML: !s.renderHTML })));

// Part C: Get the title input element and create an Observable for it
const titleInput = document.getElementById("title-input") as HTMLInputElement;
const titleStream$: Observable<Action> = fromEvent(titleInput, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, title: value })),
);

// Part C: Get the save button element and set it to trigger a save action
const saveButton = document.getElementById("save-button") as HTMLButtonElement;
const saveStream$: Observable<Action> = fromEvent(saveButton, "click").pipe(
    map(() => {
        return (s) => ({ ...s, save: true });
    }),
);

// Part D: input markdown file
const markdownFileInput = document.getElementById(
    "markdown-file-input",
) as HTMLInputElement;
const fileUploadStream$: Observable<Action> = fromEvent(
    markdownFileInput,
    "change",
).pipe(
    map((event) => (event.target as HTMLInputElement).files?.[0]),
    mergeMap((file) => {
        if (!file) return of((s: State) => s);
        // Convert the file to a stream of text
        return from(file.text()).pipe(
            map((content) => (s: State) => ({
                ...s,
                markdown: content,
                initialHTML: content, // Save the initial HTML content
            })),
        );
    }),
);
const resetFileButton = document.getElementById(
    "reset-file-button",
) as HTMLButtonElement;
const resetFileStream$: Observable<Action> = fromEvent(
    resetFileButton,
    "click",
).pipe(
    map(() => {
        // Clear the file input value
        markdownFileInput.value = "";
        return (s: State) => ({
            ...s,
            markdown: "",
            initialHTML: "",
        });
    }),
);

// Part D: Word count stream
const wordCountElement = document.getElementById(
    "word-count",
) as HTMLParagraphElement;

// Part D: Help button stream
const helpButton = document.getElementById("help-button") as HTMLButtonElement;
const helpSection = document.getElementById("help-section") as HTMLDivElement;
const helpButtonStream$: Observable<Action> = fromEvent(
    helpButton,
    "click",
).pipe(map(() => (s: State) => ({ ...s, helpVisible: !s.helpVisible }))); // Toggle the help section
// and the search input for markdown help
const searchHelpInput = document.getElementById(
    "search-help",
) as HTMLInputElement;
const helpList = document.getElementById("help-list") as HTMLUListElement;
const searchInput$: Observable<Event> = fromEvent(searchHelpInput, "input");
const filterHelpList$ = searchInput$.pipe(
    map((event) => {
        const filter = (event.target as HTMLInputElement).value.toLowerCase();
        const items = helpList.getElementsByTagName("li"); // Each example is an <li> element
        Array.from(items).forEach((item) => {
            const text = item.textContent || item.innerText;
            item.style.display = text.toLowerCase().includes(filter) // Filter the list for the search term
                ? "" // Show the item
                : "none";
        });
        return (s: State) => s;
    }),
);

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return s.save ? s : { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            // Part C: Inject the title into the HTML response directly
            const updatedHTML = data.html.replace(
                /<title>.*<\/title>/,
                `<title>${s.title}</title>`,
            );
            return {
                ...s,
                HTML: updatedHTML,
            };
        }),
        first(),
    );
}

// Part C: Save the HTML to the server
function saveHTML(s: State): Observable<State> {
    return ajax({
        url: "/api/saveHTML",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: `title=${s.title}&html=${s.HTML}`,
    }).pipe(
        map(() => ({ ...s, save: false })),
        first(),
    );
}

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,

    title: "Test",
    wordCount: 0,
    helpVisible: false,
    initialHTML: "",
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(
        input$,
        titleStream$,
        saveStream$,
        toggleRenderStream$,
        fileUploadStream$,
        helpButtonStream$,
        filterHelpList$,
        resetFileStream$,
    )
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(reducer)(resetState);
                return newReducer;
            }),
            map((action) => (s: State) => {
                // from the word count stream
                const newState = action(s);
                const strippedMarkdown = newState.markdown
                    .replace(/(^|\s)(#+|\d+\.)/g, '') // Remove hashtags and ordered list numbers
                    .replace(/(^|\s)>/g, '') // Remove blockquote symbols
                const wordCount = strippedMarkdown
                    .split(/\s+/) // Split by whitespace
                    .filter((word) => word.length > 0).length; // Filter out empty strings
                return { ...newState, wordCount };
            }),

            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                if (newState.save) {
                    return saveHTML(newState);
                }
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById(
                "html-output",
            ) as HTMLTextAreaElement | null;
            const htmlRender = document.getElementById("html-render");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                htmlOutput.textContent = value.HTML;
                htmlOutput.value = value.HTML;
            }
            if (htmlRender) {
                htmlRender.innerHTML = "";
                htmlRender.textContent = "";
                const highlight =
                    '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                htmlRender.innerHTML = highlight + value.HTML;
                // Magic code to add code highlighting
                const blocks = htmlRender.querySelectorAll("pre code");
                blocks.forEach((block) =>
                    hljs.highlightElement(block as HTMLElement),
                );
                if (!value.renderHTML) {
                    // Part D: Clear the HTML render if the toggle is off
                    htmlRender.innerHTML = "";
                    htmlRender.textContent = "";
                }
            }
            // Part D: Update the word count
            if (wordCountElement) {
                wordCountElement.textContent = `Word Count: ${value.wordCount}`;
            }
            // Part D: Update the help section visibility
            if (helpSection) {
                helpSection.style.display = value.helpVisible
                    ? "block"
                    : "none";
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
