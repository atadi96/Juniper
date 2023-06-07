# Juniper Language Server (experimental)

Implemented Language Server functionalities:
- Module member autocomplete suggestions (in some cases)
- Real-time error reporting
- Open Folder...

## Try the VS Code Extension

1. In `Program.fs` line 15, change the path to somewhere you have the Juniper stdlib files (for example `..\Juniper\junstd` from here)
1. Build the project `Juniper.LanguageServer` with F# (`dotnet build`)
1. Install npm
2. Go to the `./Client` directory
2. Run
    ```
    npm install
    npm install vscode
    npm install typescript -g
    npm run-script compile
    ```
4. Open the `./Client` folder in VS Code
5. Open `./Client/src/extension.ts`
6. Find the following code and update the path with your path to `Juniper.LanguageServer.dll`
    ```typescript
    let serverOptions: ServerOptions = {
        run: { command: serverExe, args: ['C:\\your\\path\\to\\Juniper.LanguageServer.dll'] },
        debug: { command: serverExe, args: ['C:\\your\\path\\to\\Juniper.LanguageServer.dll'] }
    }
    ```
6. Hit `F5`
7. You're ready to try the extension on `.jun` files in the VS Code Extension Development Host window that just started
