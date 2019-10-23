/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.components.text;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.Segment;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class NumberingPane extends JComponent {

    private JTextComponent textComponent;
    private Map<Position, Icon> iconMap;
    private Map<Position, List<IconListener>> listenerMap;

    public NumberingPane() {
        iconMap = new HashMap<>();
        listenerMap = new HashMap<>();
        updateUI();
    }

    @Override
    public void updateUI() {
        setUI(UIManager.getUI(this));
    }

    @Override
    public String getUIClassID() {
        return "NumberingPaneUI";
    }

    public JTextComponent getTextComponent() {
        return textComponent;
    }

    public void setTextComponent(final JTextComponent textComponent) {
        var old = this.textComponent;
        this.textComponent = textComponent;
        firePropertyChange("editorPane", old, textComponent);
    }

    public int getIconCount() {
        return iconMap.size();
    }

    public List<Map.Entry<Position, Icon>> getIconsInRange(final int startOff, final int endOff) {
        return iconMap.entrySet().stream()
                      .filter(e -> {
                          var pos = e.getKey();
                          return pos.getOffset() >= startOff && pos.getOffset() <= endOff;
                      })
                      .collect(Collectors.toList());
    }

    public Position addIconAtLine(final int lineIndex, final Icon icon) throws BadLocationException {
        return addIconAtLine(lineIndex, icon, true);
    }

    public Position addIconAtLine(final int lineIndex, final Icon icon, final boolean atTextStart) throws BadLocationException {
        int offset = textComponent.getDocument().getDefaultRootElement().getElement(lineIndex).getStartOffset();
        if (atTextStart) {
            var doc = textComponent.getDocument();
            var txt = new Segment();
            txt.setPartialReturn(true);
            String str = doc.getText(offset, 1);
            while (str.isBlank()) {
                offset++;
                str = doc.getText(offset, 1);
            }
        }
        return addIconAtOffset(offset, icon);
    }

    public void addIconListener(final int offset, final IconListener listener) throws BadLocationException {
        if (textComponent == null) return;
        addIconListener(textComponent.getDocument().createPosition(offset), listener);
    }

    public void addIconListener(final Position position, final IconListener listener) {
        if (!listenerMap.containsKey(position)) {
            listenerMap.put(position, new ArrayList<>());
        }
        var list = listenerMap.get(position);
        list.add(listener);
    }

    public void removeIconListener(final int offset, final IconListener listener) throws BadLocationException {
        if (textComponent == null) return;
        removeIconListener(textComponent.getDocument().createPosition(offset), listener);
    }

    public void removeIconListener(final Position position, final IconListener listener) {
        var list = listenerMap.get(position);
        if (list != null) {
            list.remove(listener);
        }
    }

    public List<IconListener> getIconListeners(final int offset) throws BadLocationException {
        if (textComponent == null) return List.of();
        return getIconListeners(textComponent.getDocument().createPosition(offset));
    }

    public List<IconListener> getIconListeners(final Position position) {
        var list = listenerMap.get(position);
        return list != null ? list : List.of();
    }

    public List<IconListener> getIconListeners(final int startOffset, final int endOffset) {
        return listenerMap.entrySet().stream()
                          .filter(entry -> {
                              var p = entry.getKey();
                              return p.getOffset() >= startOffset && p.getOffset() <= endOffset;
                          })
                          .map(Map.Entry::getValue)
                          .flatMap(List::stream)
                          .collect(Collectors.toList());
    }



    public Position addIconAtOffset(final int offset, final Icon icon) throws BadLocationException {
        var doc = textComponent.getDocument();
        var pos = doc.createPosition(offset);
        if (icon != null) {
            iconMap.put(pos, icon);
        }
        firePropertyChange("icons", null, icon);
        return pos;
    }

    public Collection<Icon> getIcons() {
        return iconMap.values();
    }

    public void removeIconAt(final Position position) {
        var icon = iconMap.remove(position);
        firePropertyChange("icons", icon, icon);
    }
}
