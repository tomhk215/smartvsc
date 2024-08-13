import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
    const moveHistory: { docId: string, char: string, command: string }[] = [];

    // Register smartNextChar command
    // This command moves the cursor to the next character
    // This command will skip over consecutive spaces, tabs and newlines
    const smartNextChar = vscode.commands.registerCommand('extension.smartNextChar', () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) { return; }

        const document = editor.document;
        const position = editor.selection.active;
        const docId = document.uri.toString();

        let nextChar: string;
        let nextPosition: vscode.Position;

        const currentLine = document.lineAt(position.line);
        const isEndOfLine = position.character === currentLine.range.end.character;

        if (isEndOfLine) {
            if (position.line === document.lineCount - 1) {
                nextPosition = position;
                nextChar = 'EOF';
            } else {
                nextPosition = new vscode.Position(position.line + 1, 0);
                nextChar = 'NL';
            }
        } else {
            nextPosition = position.translate(0, 1);
            nextChar = document.getText(new vscode.Range(position, nextPosition));
        }

        if (moveHistory.length >= 2) {
            const [firstMove, secondMove] = moveHistory.slice(-2);
            const isSmartNextChar = firstMove.command === 'smartNextChar' && secondMove.command === 'smartNextChar';
            const isSameChar = firstMove.char === secondMove.char;
            const isSameDoc = firstMove.docId === docId && secondMove.docId === docId;

            if (isSmartNextChar && isSameChar && isSameDoc) {
                const char = firstMove.char;
                if (char === '\t' || char === ' ') {
                    while (document.getText(new vscode.Range(nextPosition, nextPosition.translate(0, 1))) === char) {
                        nextPosition = nextPosition.translate(0, 1);
                    }
                } else if (char === 'NL') {
                    while (nextPosition.line < document.lineCount && document.lineAt(nextPosition.line).isEmptyOrWhitespace) {
                        nextPosition = nextPosition.translate(1, 0);
                    }
                }
            }
        }

        editor.selection = new vscode.Selection(nextPosition, nextPosition);
        moveHistory.push({ docId: docId, char: nextChar, command: 'smartNextChar' });
        if (moveHistory.length > 2) {
            moveHistory.shift();
        }
    });

    // Add event listener for mouse clicks
    // Clear move history when mouse is used to change selection
    vscode.window.onDidChangeTextEditorSelection((event) => {
        if (event.kind === vscode.TextEditorSelectionChangeKind.Mouse) {
            moveHistory.length = 0; // Clear move history
        }
    });

    context.subscriptions.push(smartNextChar);
}

export function deactivate() {}