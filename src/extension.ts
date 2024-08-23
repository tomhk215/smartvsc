import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
    let lastMove: { docId: string, char: string, command: string } | null = null;
    let secondLastMove: { docId: string, char: string, command: string } | null = null;

    /**
     * Command: smartNextChar
     * This command moves the cursor to the next character.
     * It skips over consecutive spaces, tabs, or newlines if the user has already moved over the same character type.
     * - If the cursor is at the end of the line, it moves to the start of the next line.
     * - If the cursor is at the end of the document, it does not move further.
     * - If consecutive spaces or tabs are detected, it skips all of them in one move.
     * - If consecutive newlines are detected, it skips to the next non-empty line.
     */
    const smartNextChar = vscode.commands.registerCommand('extension.smartNextChar', () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) return;

        const document = editor.document;
        const position = editor.selection.active;
        const docId = document.uri.toString();

        const currentLine = document.lineAt(position.line);
        const isEndOfLine = position.character === currentLine.range.end.character;

        let nextChar: string;
        let nextPosition: vscode.Position;

        if (isEndOfLine) {
            if (position.line === document.lineCount - 1) {
                // At the end of the document
                nextPosition = position;
                nextChar = 'EOF';
            } else {
                // Move to the beginning of the next line
                nextPosition = new vscode.Position(position.line + 1, 0);
                nextChar = 'NL';
            }
        } else {
            nextPosition = position.translate(0, 1);
            nextChar = document.getText(new vscode.Range(position, nextPosition));
        }

        // Check for repeating moves (optimized to avoid history slicing and repeated calls)
        if (lastMove && secondLastMove) {
            const isSmartNextChar = lastMove.command === 'smartNextChar' && secondLastMove.command === 'smartNextChar';
            const isSameChar = lastMove.char === secondLastMove.char;
            const isSameDoc = lastMove.docId === docId && secondLastMove.docId === docId;

            if (isSmartNextChar && isSameChar && isSameDoc) {
                const char = lastMove.char;
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

        // Shift move history efficiently
        secondLastMove = lastMove;
        lastMove = { docId, char: nextChar, command: 'smartNextChar' };
    });

    /**
     * Command: smartPreviousChar
     * This command moves the cursor to the previous character.
     * It skips over consecutive spaces, tabs, or newlines if the user has already moved over the same character type.
     * - If the cursor is at the start of the line, it moves to the end of the previous line.
     * - If the cursor is at the beginning of the document, it does not move further.
     * - If consecutive spaces or tabs are detected, it skips all of them in one move.
     * - If consecutive newlines are detected, it skips to the previous non-empty line.
     */
    const smartPreviousChar = vscode.commands.registerCommand('extension.smartPreviousChar', () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) return;

        const document = editor.document;
        const position = editor.selection.active;
        const docId = document.uri.toString();

        const currentLine = document.lineAt(position.line);
        const isStartOfLine = position.character === 0;

        let prevChar: string;
        let prevPosition: vscode.Position;

        if (isStartOfLine) {
            if (position.line === 0) {
                // At the start of the document
                prevPosition = position;
                prevChar = 'BOF';
            } else {
                // Move to the end of the previous line
                prevPosition = new vscode.Position(position.line - 1, document.lineAt(position.line - 1).range.end.character);
                prevChar = 'NL';
            }
        } else {
            prevPosition = position.translate(0, -1);
            prevChar = document.getText(new vscode.Range(prevPosition, position));
        }

        // Check for repeating moves (optimized)
        if (lastMove && secondLastMove) {
            const isSmartPrevChar = lastMove.command === 'smartPrevChar' && secondLastMove.command === 'smartPrevChar';
            const isSameChar = lastMove.char === secondLastMove.char;
            const isSameDoc = lastMove.docId === docId && secondLastMove.docId === docId;

            if (isSmartPrevChar && isSameChar && isSameDoc) {
                const char = lastMove.char;
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

        // Shift move history efficiently
        secondLastMove = lastMove;
        lastMove = { docId, char: prevChar, command: 'smartPrevChar' };
    });

    // Event Listener: Clear history when selection is changed using the mouse
    vscode.window.onDidChangeTextEditorSelection((event) => {
        if (event.kind === vscode.TextEditorSelectionChangeKind.Mouse) {
            // Reset move history without array manipulation
            lastMove = null;
            secondLastMove = null;
        }
    });

    context.subscriptions.push(smartNextChar, smartPreviousChar);
}

export function deactivate() {}