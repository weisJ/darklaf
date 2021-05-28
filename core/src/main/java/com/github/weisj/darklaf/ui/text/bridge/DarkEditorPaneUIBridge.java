/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.ui.text.bridge;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.text.dummy.DummyEditorPane;
import com.github.weisj.darklaf.ui.text.dummy.DummyEditorPaneUI;
import com.github.weisj.darklaf.ui.text.dummy.DummyTextUIMethods;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.value.WeakShared;

/** @author Jannis Weis */
public abstract class DarkEditorPaneUIBridge extends DarkTextUI {

    private static final WeakShared<DummyEditorPane> shareDummyEditorPane = new WeakShared<>(DummyEditorPane::new);
    private static final WeakShared<DummyTextUIMethods> sharedDummyUI = new WeakShared<>(DummyEditorPaneUI::new);

    private DummyEditorPane editorPane;
    private DummyTextUIMethods basicEditorPaneUI;
    private PropertyChangeListener propertyChangeListener;

    @Override
    public void installUI(final JComponent c) {
        editorPane = createDummyEditorPane();
        basicEditorPaneUI = createDummyUI();
        editorPane.setEditorPane((JEditorPane) c);
        basicEditorPaneUI.installUI(editorPane);
        super.installUI(c);
        updateDisplayProperties();
    }

    protected DummyTextUIMethods createDummyUI() {
        return sharedDummyUI.get();
    }

    protected DummyEditorPane createDummyEditorPane() {
        return shareDummyEditorPane.get();
    }

    @Override
    protected void installListeners() {
        propertyChangeListener = editorPane.getPropertyChangeListener();
        super.installListeners();
    }

    @Override
    public void uninstallUI(final JComponent c) {
        cleanDisplayProperties(c);
        super.uninstallUI(c);
        editorPane.setEditorPane(null);
        editorPane.addPropertyChangeListener(null);
        editorPane = null;
        basicEditorPaneUI = null;
    }

    protected void updateDisplayProperties() {
        updateDisplayProperties((JEditorPane) getComponent(),
                new PropertyChangeEvent(editorPane, PropertyKey.FONT, editorPane.getFont(), editorPane.getFont()));
    }

    protected void updateDisplayProperties(final JEditorPane c, final PropertyChangeEvent event) {
        editorPane.setEditorPane(c);
        basicEditorPaneUI.propertyChange(event);
    }

    protected void cleanDisplayProperties(final JComponent c) {
        editorPane.setEditorPane((JEditorPane) c);
        basicEditorPaneUI.uninstallUI(editorPane);
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        updateActionMap();
    }

    protected void updateFocusAcceleratorBinding(final PropertyChangeEvent event) {
        /*
         * This invokes the UpdateHandler propertyChange event which in turn invokes
         * #updateFocusAcceleratorBinding
         */
        propertyChangeListener.propertyChange(event);
    }

    protected void updateActionMap() {
        editorPane.setActionMap(new ActionMap());
        editorPane.setEditorPane((JEditorPane) getComponent());
        basicEditorPaneUI.installKeyBoardActionsReal();
        ActionMap map = editorPane.getActionMap();
        SwingUtilities.replaceUIActionMap(getComponent(), map);
        installDarkKeyBoardActions();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String name = evt.getPropertyName();
        if ("editorKit".equals(name) || PropertyKey.EDITABLE.equals(name)) {
            editorPane.setEditorPane((JEditorPane) getComponent());
            basicEditorPaneUI.propertyChange(evt);
        } else if (PropertyKey.FOREGROUND.equals(name) || PropertyKey.FONT.equals(name)
                || PropertyKey.DOCUMENT.equals(name) || JEditorPane.W3C_LENGTH_UNITS.equals(name)
                || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)) {
            updateDisplayProperties((JEditorPane) getComponent(), evt);
        } else if ("focusAccelerator".equals(name)) {
            updateFocusAcceleratorBinding(evt);
        }
    }

    @Override
    public EditorKit getEditorKit(final JTextComponent tc) {
        JEditorPane pane = (JEditorPane) getComponent();
        return pane.getEditorKit();
    }
}
