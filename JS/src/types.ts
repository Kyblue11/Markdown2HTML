export type State = Readonly<{
    markdown: string;
    HTML: string;
    renderHTML: boolean;
    save: boolean;

    title: string; // title tag in the HTML
    wordCount: number; // Part D: Word count
    helpVisible: boolean; // Part D: Help button
    initialHTML: string; // Part D: input markdown file
}>;
