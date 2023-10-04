# Ham Tools

Various tools to work with [Ham](https://github.com/prenaux/ham). You can find it in the [VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=prenaux.ham-tools).

## Commands

- `ham.swiper-word-at-cursor`: search the current word at cursor.
- `ham.swiper`: search the last value tried.

## ham.swiper

A copy of emacs swiper, the thing I missed the most in vscode. Supports regex,
negate, match highting, case sensitive search.

Search `"swiper !not Test"` matches a line with `SWiPeR Test` without mentions
of `not` string.

Invoke command `Swiper: Swiper Search/Resume` and start typing.

Basic rules:
1. Search either literal string e.g. `abc` or javascript regex `/.../`.
2. Search strings separated by space are AND-ed together. e.g. "a b" matched lines with "a" and "b" on the same line.
3. Search string prefixed with `!` negates the search, "a !b" matches lines with "a" but not "b". Use regex `/\!/` if you want to match `"!"` literal string.
   Search string starts with `!` does not contribute to the border highlights.
4. Default case insensitive search. Upcased search term matches case sensitively.

Some quick Example:

```sh
# search wildcard
/command.*swiper/

a b matches lines with a and b
a !b # matches lines with a but not b

/lint|display/ # lint OR display
lint|display # searches the literal string "lint|display"

# line starts with test
/^test/

a B # matches "aB", "AB"

/\(.*\)/ # matches paren
!/\(.*\)/ # do not match paren
```

## Configuration

- `ham.swiper.selectMatch`: (default true) controls if the picked match is selected. Set to false, cursor jumps to the end of the match word.

## Dev

Setup:
```
cd $HAM_HOME/sources/vscode/ext-swiper
. hat
npm install
```

- Open the extension's directory in VS Code
- Press F5 or go to Run > Start Debugging in VS Code.
- This will open a new VS Code window titled [Extension Development Host] where the extension is loaded.

Packaging and Publishing (cf https://code.visualstudio.com/api/working-with-extensions/publishing-extension#vsce):
```
npm install -g @vscode/vsce
vsce package && vsce publish
```

You'll need a Personal Acces Token to publish:
- Get ready for a process that'd make every bureaucrats proud
- Create an org: https://learn.microsoft.com/en-us/azure/devops/organizations/accounts/create-organization?view=azure-devops
- Get the token: https://code.visualstudio.com/api/working-with-extensions/publishing-extension#get-a-personal-access-token
- Note that the documentation says that you must create a publisher, that will
  likely fail with a 403. But you dont actually need to create a publisher
  just use your personal access token and make sure to use the same org name
  as you used when creating the token.

## Credits

Started as a fork of the [Swiper](https://github.com/wenhoujx/swiper) extension.
