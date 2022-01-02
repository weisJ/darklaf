/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.weisj.darklaf.components.text;

import java.awt.Font;
import java.util.*;
import java.util.stream.Collectors;

import javax.swing.*;
import javax.swing.text.*;

import com.github.weisj.darklaf.util.StringUtil;

public class NumberingPane extends JComponent {

    public static final String KEY_EDITOR = "editorPane";
    public static final String KEY_ICONS = "icons";
    public static final String KEY_MIN_ICON_WIDTH = "minimumIconWidth";
    private JTextComponent textComponent;
    private final Map<Position, Icon> iconMap;
    private final Map<Position, List<IconListener>> listenerMap;
    private int width;

    public NumberingPane() {
        iconMap = new HashMap<>();
        listenerMap = new HashMap<>();
        updateUI();
    }

    @Override
    public void setFont(Font font) {
        super.setFont(font);
    }

    @Override
    public void updateUI() {
        setUI(UIManager.getUI(this));
        if (textComponent != null) {
            firePropertyChange(KEY_EDITOR, textComponent, null);
            firePropertyChange(KEY_EDITOR, null, textComponent);
        }
    }

    @Override
    public String getUIClassID() {
        return "NumberingPaneUI";
    }

    public JTextComponent getTextComponent() {
        return textComponent;
    }

    public void setTextComponent(final JTextComponent textComponent) {
        JTextComponent old = this.textComponent;
        this.textComponent = textComponent;
        firePropertyChange(KEY_EDITOR, old, textComponent);
    }

    public int getIconCount() {
        return iconMap.size();
    }

    public List<Map.Entry<Position, Icon>> getIconsInRange(final int startOff, final int endOff) {
        return iconMap.entrySet().stream().filter(e -> {
            Position pos = e.getKey();
            return pos.getOffset() >= startOff && pos.getOffset() <= endOff;
        }).collect(Collectors.toList());
    }

    public Position addIconAtLine(final int lineIndex, final Icon icon) throws BadLocationException {
        return addIconAtLine(lineIndex, icon, true);
    }

    public Position addIconAtLine(final int lineIndex, final Icon icon, final boolean atTextStart)
            throws BadLocationException {
        int offset = textComponent.getDocument().getDefaultRootElement().getElement(lineIndex).getStartOffset();
        if (atTextStart) {
            Document doc = textComponent.getDocument();
            Segment txt = new Segment();
            txt.setPartialReturn(true);
            String str = doc.getText(offset, 1);
            while (StringUtil.isBlank(str)) {
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

    public Position addIconAtOffset(final int offset, final Icon icon) throws BadLocationException {
        Document doc = textComponent.getDocument();
        Position pos = doc.createPosition(offset);
        if (icon != null) {
            iconMap.put(pos, icon);
        }
        firePropertyChange(KEY_ICONS, null, icon);
        return pos;
    }

    public void removeIconListener(final int offset, final IconListener listener) throws BadLocationException {
        if (textComponent == null) return;
        removeIconListener(textComponent.getDocument().createPosition(offset), listener);
    }

    public void addIconListener(final Position position, final IconListener listener) {
        if (!listenerMap.containsKey(position)) {
            listenerMap.put(position, new ArrayList<>());
        }
        List<IconListener> list = listenerMap.get(position);
        list.add(listener);
    }

    public void removeIconListener(final Position position, final IconListener listener) {
        List<IconListener> list = listenerMap.get(position);
        if (list != null) {
            list.remove(listener);
        }
    }

    public List<IconListener> getIconListeners(final int offset) throws BadLocationException {
        if (textComponent == null) return new ArrayList<>();
        return getIconListeners(textComponent.getDocument().createPosition(offset));
    }

    public List<IconListener> getIconListeners(final Position position) {
        List<IconListener> list = listenerMap.get(position);
        return list != null ? list : new ArrayList<>();
    }

    public List<IconListener> getIconListeners(final int startOffset, final int endOffset) {
        return listenerMap.entrySet().stream().filter(entry -> {
            Position p = entry.getKey();
            return p.getOffset() >= startOffset && p.getOffset() <= endOffset;
        }).map(Map.Entry::getValue).flatMap(List::stream).collect(Collectors.toList());
    }

    public void addIndexListener(final IndexListener listener) {
        listenerList.add(IndexListener.class, listener);
    }

    public void removeIndexListener(final IndexListener listener) {
        listenerList.remove(IndexListener.class, listener);
    }

    public int getMinimumIconWidth() {
        return width;
    }

    public void setMinimumIconWidth(final int width) {
        int old = this.width;
        this.width = Math.max(width, 0);
        firePropertyChange(KEY_MIN_ICON_WIDTH, old, width);
    }

    public Collection<Icon> getIcons() {
        return iconMap.values();
    }

    public Icon getIcon(final Position position) {
        return iconMap.get(position);
    }

    public void removeIconAt(final Position position) {
        Icon icon = iconMap.remove(position);
        firePropertyChange(KEY_ICONS, icon, icon);
    }

    public List<IconListener> getIconListeners() {
        return listenerMap.values().stream().flatMap(List::stream).collect(Collectors.toList());
    }

    public IndexListener[] getIndexListeners() {
        return listenerList.getListeners(IndexListener.class);
    }
}
