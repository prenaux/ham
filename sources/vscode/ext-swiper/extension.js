const vscode = require('vscode');

const isDebug = false

const MATCH_BORDER_STYLE_ACTIVE = 'solid';
const MATCH_BORDER_STYLE_OTHERS = 'none none solid none'; // 'none none solid none';

// TODO: not sure how to select the best list of colors according to the theme.
const styles = [
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_ACTIVE,
      borderWidth: 'medium',
      borderColor: "orange",
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "orange"
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "cyan"
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "green"
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "yellow"
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "BlueViolet"
    }
  ),
  vscode.window.createTextEditorDecorationType(
    {
      borderStyle: MATCH_BORDER_STYLE_OTHERS,
      borderWidth: 'medium',
      borderColor: "Fuchsia"
    }
  ),
]



const PROMPT_STRING = "type 2 or more chars to search"
const CURRENT_LINE_LABEL = "<current line>"

let state = {
  // last searched string
  lastValue: PROMPT_STRING,
  // last selected item
  lastSelected: null
}


function _parseSearchString(searchStr) {
  if (!searchStr.trim().length) {
    return []
  }
  return searchStr.split(" ")
    .map(subSearch => subSearch.trim())
    .filter(subSearch => subSearch)
    .map(subSearch => {
      const isNegate = subSearch.startsWith("!")
      return ({
        pattern: isNegate ? subSearch.slice(1) : subSearch,
        isRegex: isNegate ? subSearch.startsWith("!/") : subSearch.startsWith("/"),
        caseSensitive: /[A-Z]/.test(subSearch),
        negate: subSearch.startsWith("!")
      })
    })
}

function _searchContent(parsed) {
  const items = []
  const doc = vscode.window.activeTextEditor.document
  for (let i = 0; i < doc.lineCount; i++) {
    const matches = _searchLine(i, doc.lineAt(i).text, parsed)
    if (matches) {
      items.push(matches)
    }
  }
  return items
}

function _searchLine(lineIndex, line, parsed) {
  const matchedRange = {
    line: lineIndex,
    ranges: []
  }
  for (const p of parsed) {
    if (p.isRegex) {
      const splitRegex = p.pattern.match(new RegExp('^/(.*?)/([gimy]*)$'));
      if (!splitRegex) {
        return null
      }
      const [pattern, flags] = splitRegex.slice(1)
      // only find the first
      const regex = new RegExp(pattern, flags);
      const m = regex.exec(line)
      if (!m && !p.negate) {
        // regular mode, and this line doesn't match
        return null
      } else if (m && p.negate) {
        // intentionally skip for case when matches but should be ignored.
        return null
      } else if (!m && p.negate) {
        // negate, and doesn't match, should keep this line.
        continue
      } else {
        // normal mode, record the matched range.
        matchedRange.ranges.push([m.index, m[0].length])
      }
    } else {
      const m = p.caseSensitive ? line.indexOf(p.pattern) : line.toLowerCase().indexOf(p.pattern)
      if (p.negate) {
        if (m !== -1) {
          // intentionally skip this line.
          return null
        }
      } else {
        if (m === -1) {
          // normal mode, no match
          return null
        } else {
          // normal mode, matches, record range.
          matchedRange.ranges.push([m, p.pattern.length])
        }
      }
    }
  }
  return matchedRange
}

function _search(searchStr, pick) {
  if (searchStr.length < 2 || searchStr === PROMPT_STRING) {
    // to avoid search on too short a string.
    return
  }
  const parsed = _parseSearchString(searchStr)
  const items = _searchContent(parsed)
  isDebug && console.log(searchStr)
  isDebug && console.log(JSON.stringify(parsed))
  isDebug && console.log(JSON.stringify(items))

  const doc = vscode.window.activeTextEditor.document
  pick.items = items.map(match => ({
    label: `${match.line}: ${doc.lineAt(match.line).text}`,
    // force vscode quickpick to match the description instead of the line content itself.
    // otherwise quickpick filters to nothing.
    description: searchStr,
    ...match
  }))

  var matchingItem = undefined;
  if (state.lastValue === searchStr && state.lastSelected) {
    // First, try to find an exact match
    matchingItem = pick.items.find(it =>
      (CURRENT_LINE_LABEL === state.lastSelected.label || it.label === state.lastSelected.label) &&
      it.line === state.lastSelected.line
    );

    // If an exact match is not found, find the nearest line
    if (!matchingItem && state.lastSelected.line) {
      matchingItem = pick.items.reduce((closest, current) => {
        let currentDifference = Math.abs(current.line - state.lastSelected.line);
        let closestDifference = Math.abs(closest.line - state.lastSelected.line);

        return currentDifference < closestDifference ? current : closest;
      });
    }

    if (matchingItem) {
      pick.activeItems = [matchingItem];
    }
  }
  _updateMatchColor(items, matchingItem)
}

function _clearDecorations() {
  styles.forEach(s => vscode.window.activeTextEditor.setDecorations(s, []))
}

function _updateMatchColor(items, matchingItem) {
  _clearDecorations()
  const colors = Array.from(Array(styles.length), () => [])
  for (const item of items) {
    for (let i = 0; i < item.ranges.length; i++) {
      const [start, length] = item.ranges[i]
      if (length === 0) {
        // no length, no need to set border.
        continue
      }

      let colorIndex = (matchingItem && item.line === matchingItem.line) ?
        0 : ((i + 1) % styles.length)
      colors[colorIndex].push(
        new vscode.Range(
          new vscode.Position(item.line, start),
          new vscode.Position(item.line, start + length)
        )
      )
    }
  }
  for (let i = 0; i < colors.length; i++) {
    vscode.window.activeTextEditor.setDecorations(styles[i], colors[i])
  }
}


function _jumpTo(selected) {
  // find the last range which corresponds to the last search term
  const lastIndex = _firstOrNull(selected.ranges.reverse())
  const start = lastIndex ? lastIndex[0] : 0
  const end = lastIndex ? lastIndex[0] + lastIndex[1] : 0
  const selectMatch = vscode.workspace.getConfiguration("ham.swiper").get("selectMatch")
  vscode.window.activeTextEditor.selections = [
    new vscode.Selection(
      new vscode.Position(selected.line, selectMatch ? start : end),
      new vscode.Position(selected.line, end))]
}

function _firstOrNull(items) {
  if (!(items.length) || !(items[0])) {
    return null
  } else {
    return items[0]
  }
}

function swipeWordAtCursor() {
  const editor = vscode.window.activeTextEditor
  const currentSelection = editor.selection
  const wordRange = editor.document.getWordRangeAtPosition(currentSelection.start)
  // either selection or cursor
  const word = editor.document.getText(editor.selection) || (wordRange ? editor.document.getText(wordRange) : state.lastValue)
  const line = currentSelection.start.line
  state = {
    lastValue: word, // set last value with current word or selection
    lastSelected: { line, label: CURRENT_LINE_LABEL }
  }
  swipe()
}

function swipe() {
  const currentSelection = vscode.window.activeTextEditor.selection
  const pick = vscode.window.createQuickPick()

  pick.canSelectMany = false
  pick.matchOnDescription = true
  pick.value = state.lastValue

  pick.onDidChangeValue((value) => {
    _search(value, pick)
  })
  pick.onDidAccept(() => {
    const selected = _firstOrNull(pick.selectedItems)
    isDebug && console.log(`selected: ${JSON.stringify(selected)}`)
    if (!selected) {
      return
    }
    state = {
      lastValue: pick.value,
      lastSelected: selected
    }
    pick.hide();
    _jumpTo(state.lastSelected)
  });
  pick.onDidChangeActive(items => {
    const focused = _firstOrNull(items)
    if (!focused) {
      return
    }
    _focusOnActiveItem(focused)
  })
  pick.onDidHide(() => {
    // resort previous cursor position if nothing selected
    _clearDecorations()
    _resortCursorIfNoneSelected(pick, currentSelection)
  })
  pick.show()
}

function _resortCursorIfNoneSelected(pick, previousSelection) {
  if (pick.selectedItems.length === 0) {
    vscode.window.activeTextEditor.revealRange(
      new vscode.Range(previousSelection.start, previousSelection.end),
      vscode.TextEditorRevealType.InCenter);
    vscode.window.activeTextEditor.selection = previousSelection;
  }
}

function _focusOnActiveItem(focused) {
  const start = new vscode.Position(focused.line, 0);
  const end = new vscode.Position(focused.line, 0);
  vscode.window.activeTextEditor.revealRange(
    new vscode.Range(start, end), vscode.TextEditorRevealType.InCenter);
  vscode.window.activeTextEditor.selection = new vscode.Selection(start, end);
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand('ham.swiper', () => swipe()));
  context.subscriptions.push(
    vscode.commands.registerCommand('ham.swiper-word-at-cursor', () =>
      swipeWordAtCursor()
    ));
}

function deactivate() { }

module.exports = {
  activate,
  deactivate
}