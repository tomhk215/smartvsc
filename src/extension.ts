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

    // Register smartPrevChar command
    // This command moves the cursor to the previous character
    // This command will skip over consecutive spaces, tabs and newlines
    const smartPreviousChar = vscode.commands.registerCommand('extension.smartPreviousChar', () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) { return; }

        const document = editor.document;
        const position = editor.selection.active;
        const docId = document.uri.toString();

        let prevChar: string;
        let prevPosition: vscode.Position;

        const currentLine = document.lineAt(position.line);
        const isStartOfLine = position.character === 0;

        if (isStartOfLine) {
            if (position.line === 0) {
                prevPosition = position;
                prevChar = 'BOF';
            } else {
                prevPosition = new vscode.Position(position.line - 1, document.lineAt(position.line - 1).range.end.character);
                prevChar = 'NL';
            }
        } else {
            prevPosition = position.translate(0, -1);
            prevChar = document.getText(new vscode.Range(prevPosition, position));
        }

        if (moveHistory.length >= 2) {
            const [firstMove, secondMove] = moveHistory.slice(-2);
            const isSmartPrevChar = firstMove.command === 'smartPrevChar' && secondMove.command === 'smartPrevChar';
            const isSameChar = firstMove.char === secondMove.char;
            const isSameDoc = firstMove.docId === docId && secondMove.docId === docId;

            if (isSmartPrevChar && isSameChar && isSameDoc) {
                const char = firstMove.char;
                if (char === '\t' || char === ' ') {
                    while (prevPosition.character > 0 && document.getText(new vscode.Range(prevPosition, prevPosition.translate(0, -1))) === char) {
                        prevPosition = prevPosition.translate(0, -1);
                    }
                } else if (char === 'NL') {
                    while (prevPosition.line > 0 && document.lineAt(prevPosition.line).isEmptyOrWhitespace) {
                        prevPosition = prevPosition.translate(-1, 0);
                    }
                    prevPosition = new vscode.Position(prevPosition.line, document.lineAt(prevPosition.line).range.end.character);
                }
            }
        }

        editor.selection = new vscode.Selection(prevPosition, prevPosition);
        moveHistory.push({ docId: docId, char: prevChar, command: 'smartPrevChar' });
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

    context.subscriptions.push(smartNextChar, smartPreviousChar);

}

export function deactivate() {}