import * as vscode from 'vscode';

const DEBUG = true;

export function activate(context: vscode.ExtensionContext) {
    let outputChannel: vscode.OutputChannel | undefined;
    if (DEBUG) {		
        outputChannel = vscode.window.createOutputChannel("SmartVSC");
        outputChannel.show(true);
    }

    let moveHistory: { docId: string, char: string }[] = [
        { docId: '', char: '' },
        { docId: '', char: '' }
    ];

    let smartNextChar = vscode.commands.registerCommand('extension.smartNextChar', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            const document = editor.document;
            const selection = editor.selection;
            const position = selection.active;
            const docId = document.uri.toString();

            const nextPosition = position.translate(0, 1);
            let nextChar: string;
            if (position.character === document.lineAt(position.line).range.end.character) {
                nextChar = 'NL';
            } else {
                nextChar = document.getText(new vscode.Range(position, nextPosition));
            }

            if (DEBUG && outputChannel !== undefined) {
                outputChannel.appendLine(
                    `Executed Command 'SmartNextChar'\t| ` +
                    `Move History: '${moveHistory[0].char}'-'${moveHistory[1].char}'\t| ` +
                    `Current Char: '${nextChar}'`);
            }

            if (moveHistory[0].char === '\t' && moveHistory[1].char === '\t' && nextChar === '\t' &&
                moveHistory[0].docId === docId && moveHistory[1].docId === docId) {
                // Skip all tabs
                let newPosition = position;
                while (document.getText(new vscode.Range(newPosition, newPosition.translate(0, 1))) === '\t') {
                    newPosition = newPosition.translate(0, 1);
                }
                editor.selection = new vscode.Selection(newPosition, newPosition);
                moveHistory.shift();
                moveHistory.push({ docId: docId, char: '\t' });
            } else if (moveHistory[0].char === 'SP' && moveHistory[1].char === 'SP' && nextChar === ' ' &&
                moveHistory[0].docId === docId && moveHistory[1].docId === docId) {
                // Skip all spaces
                let newPosition = position;
                while (document.getText(new vscode.Range(newPosition, newPosition.translate(0, 1))) === ' ') {
                    newPosition = newPosition.translate(0, 1);
                }
                editor.selection = new vscode.Selection(newPosition, newPosition);
                moveHistory.shift();
                moveHistory.push({ docId: docId, char: 'SP' });
            } 
            else if (position.character === document.lineAt(position.line).range.end.character) {
                    // Move to the start of the next line if at the end of the current line
                    let nextLine = position.line + 1;
                    if (moveHistory[0].docId === docId && moveHistory[1].docId === docId &&
                        moveHistory[0].char === 'NL' && moveHistory[1].char === 'NL') {
                        if (nextLine < document.lineCount) {
                            while (nextLine < document.lineCount && document.lineAt(nextLine).isEmptyOrWhitespace) {
                                nextLine++; // Skip empty lines
                            }
                        }
                    }
                            
                    const newPosition = new vscode.Position(nextLine, 0);
                    editor.selection = new vscode.Selection(newPosition, newPosition);
                    moveHistory.shift();
                    moveHistory.push({ docId: docId, char: 'NL' });
            } 
            else {
                editor.selection = new vscode.Selection(nextPosition, nextPosition);
                moveHistory.shift();
                moveHistory.push({ docId: docId, char: document.getText(new vscode.Range(position, nextPosition))});
            }
        }
    });

    context.subscriptions.push(smartNextChar);
}

export function deactivate() {}