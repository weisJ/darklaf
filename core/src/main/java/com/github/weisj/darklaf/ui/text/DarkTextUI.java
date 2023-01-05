/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTextUI;
import javax.swing.text.*;

import com.github.weisj.darklaf.Customization;
import com.github.weisj.darklaf.components.border.MarginBorderWrapper;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.swingdsl.VisualPaddingListener;
import com.github.weisj.darklaf.ui.OpacityBufferedUI;
import com.github.weisj.darklaf.ui.cell.DarkCellBorder;
import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.ui.text.action.DarkKeyTypedAction;
import com.github.weisj.darklaf.ui.text.action.ToggleInsertAction;
import com.github.weisj.darklaf.ui.text.popup.DarkTextPopupMenu;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.ui.tree.DarkTreeUI;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public abstract class DarkTextUI extends BasicTextUI implements OpacityBufferedUI, Customization.Text {

    public static final String KEY_IS_TREE_EDITOR = DarkTreeUI.KEY_IS_TREE_EDITOR;
    public static final String KEY_IS_TABLE_EDITOR = DarkTableUI.KEY_IS_TABLE_EDITOR;
    public static final String KEY_IS_LIST_EDITOR = DarkListUI.KEY_IS_LIST_EDITOR;

    protected static final String TOGGLE_INSERT = "toggle_insert";

    protected DarkTextListener textListener;
    protected VisualPaddingListener visualPaddingListener;

    protected JTextComponent editor;
    protected DefaultTextRenderer defaultTextRenderer;
    protected DarkCaret darkCaret;
    protected Insets margins;

    @Override
    protected Caret createCaret() {
        return getDarkCaret();
    }

    protected DarkCaret getDarkCaret() {
        if (darkCaret == null) darkCaret = createDarkCaret();
        return darkCaret;
    }

    protected DarkCaret createDarkCaret() {
        DarkCaret c = new DarkCaret(getDefaultCaretStyle(), getDefaultInsertCaretStyle());
        c.setLineExtendingEnabled(PropertyUtil.getBooleanProperty(editor, KEY_EXTEND_LINE_SELECTION));
        c.setRoundedSelectionEdges(PropertyUtil.getBooleanProperty(editor, KEY_ROUNDED_SELECTION));
        return c;
    }

    protected abstract DarkCaret.CaretStyle getDefaultCaretStyle();

    protected DarkCaret.CaretStyle getDefaultInsertCaretStyle() {
        return DarkCaret.CaretStyle.BLOCK_STYLE;
    }

    protected Color disabledColor;
    protected Color inactiveColor;
    protected boolean uninstalling;

    @Override
    protected void installDefaults() {
        super.installDefaults();
        if (editor != null) {
            PropertyUtil.installProperty(editor, ToolTipConstants.KEY_STYLE, ToolTipStyle.PLAIN);
            PropertyUtil.installBooleanProperty(editor, KEY_ROUNDED_SELECTION, "TextComponent.roundedSelection");
            PropertyUtil.installBooleanProperty(editor, KEY_EXTEND_LINE_SELECTION,
                    getPropertyPrefix() + ".extendSelection");
        }
        disabledColor = UIManager.getColor(getPropertyPrefix() + ".disabledBackground");
        inactiveColor = UIManager.getColor(getPropertyPrefix() + ".inactiveBackground");

        margins = UIManager.getInsets(getPropertyPrefix() + ".margins");

        updateMargins();
        installBorder();
        installPopupMenu();
        updateBackground(editor);
    }


    protected void updateMargins() {
        Insets margin = editor.getMargin();
        if (margin == null || margin instanceof UIResource) {
            editor.setMargin(isInCell(editor) ? new InsetsUIResource(0, 0, 0, 0) : margins);
        }
    }

    public static boolean isBorderlessTextField(final JTextComponent textComponent) {
        if (textComponent == null) return false;
        String className = textComponent.getClass().getName();
        return "javax.swing.plaf.basic.BasicComboBoxEditor$BorderlessTextField".equals(className);
    }

    public static boolean isSpinnerEditor(final JTextComponent textComponent) {
        if (textComponent == null) return false;
        return DarkUIUtil.getParentOfType(JSpinner.class, textComponent, 2) != null;
    }

    protected void installPopupMenu() {
        JPopupMenu popupMenu = editor.getComponentPopupMenu();
        if (popupMenu == null || popupMenu instanceof UIResource) {
            editor.setComponentPopupMenu(createPopupMenu(editor));
        }
    }

    protected JPopupMenu createPopupMenu(final JTextComponent textComponent) {
        return new DarkTextPopupMenu(textComponent);
    }

    protected void uninstallPopupMenu() {
        JPopupMenu popupMenu = editor.getComponentPopupMenu();
        if (popupMenu instanceof UIResource) {
            editor.setComponentPopupMenu(null);
        }
    }

    protected void installBorder() {
        if (isBorderlessTextField(editor)) {
            // OpenJDK BorderlessTextField has a bug with its setBorder implementation,
            // so we reset the border
            // See https://mail.openjdk.java.net/pipermail/swing-dev/2020-March/010226.html
            if (editor.getBorder() != null) editor.setBorder(null);
            return;
        }
        // Prevent BasicSpinnerUI from entering a recursion loop setting the border to null in response
        // to us wrapping it with a MarginBorder.
        if (isSpinnerEditor(editor) && !UIManager.getBoolean("Spinner.editorBorderPainted")) {
            return;
        }
        if (uninstalling) return;
        MarginBorderWrapper.installBorder(editor);
    }

    protected void uninstallBorder() {
        MarginBorderWrapper.uninstallBorder(editor);
    }

    protected DefaultTextRenderer createDefaultTextRenderer() {
        return new LabelDefaultTextRenderer();
    }

    @Override
    protected void uninstallDefaults() {
        uninstalling = true;
        uninstallBorder();
        uninstallPopupMenu();
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        textListener = createTextListener();
        editor.addFocusListener(textListener);
        editor.addPropertyChangeListener(textListener);
        visualPaddingListener = new VisualPaddingListener();
        editor.addPropertyChangeListener(visualPaddingListener);
    }

    protected DarkTextListener createTextListener() {
        return new DarkTextListener(editor, this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        editor.removeFocusListener(textListener);
        editor.removePropertyChangeListener(textListener);
        textListener = null;
        editor.removePropertyChangeListener(visualPaddingListener);
        visualPaddingListener = null;
    }

    protected void updateBackground(final JTextComponent c) {
        PropertyUtil.installBackground(c, getBackground(c));
    }

    protected Color getBackground(final JTextComponent c) {
        if (!c.isEnabled()) {
            return disabledColor;
        } else if (!c.isEditable()) {
            return inactiveColor;
        } else {
            return c.getBackground();
        }
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        Dimension dim = super.getPreferredSize(c);
        if (isEmpty()) {
            String defaultText = getDefaultText();
            if (!defaultText.isEmpty()) {
                Component renderer = getDefaultTextRenderer().getRendererComponent(editor, defaultText);
                renderer.setFont(editor.getFont());
                Dimension prefSize = renderer.getPreferredSize();
                Insets i = c.getInsets();
                prefSize.width += i.left + i.right;
                prefSize.height += i.top + i.bottom;
                dim.width = Math.max(dim.width, prefSize.width);
                dim.height = Math.max(dim.height, prefSize.height);
            }
        }
        if (c.getSize().width < dim.width) {
            // Provide spacing for caret to avoid jumping text if the caret moves to the start or end of the
            // line.
            dim.width += getCaretWidth(editor);
        }
        return dim;
    }

    public static int getCaretWidth(final JTextComponent textComponent) {
        Caret caret = textComponent.getCaret();
        if (caret instanceof DefaultCaret) {
            return (int) ((DefaultCaret) caret).getWidth();
        }
        return 1;
    }

    @Override
    protected void paintBackground(final Graphics g) {
        final Container parent = getRelevantParent(editor);
        if (parent != null && !(parent instanceof Window)) {
            g.setColor(parent.getBackground());
            if (parent instanceof JTextComponent) {
                if (!parent.isEnabled()) {
                    g.setColor(disabledColor);
                } else if (!((JTextComponent) parent).isEditable()) {
                    g.setColor(inactiveColor);
                }
            }
            g.fillRect(0, 0, editor.getWidth(), editor.getHeight());
        }

        if (!editor.isEnabled()) {
            return;
        }

        if (editor.isOpaque() && isInCell(editor)) {
            g.fillRect(0, 0, editor.getWidth(), editor.getHeight());
        }

        Border border = getBorder(editor);

        g.setColor(editor.getBackground());
        if (border instanceof DarkTextBorder) {
            paintBorderBackground((Graphics2D) g, editor, (DarkTextBorder) border);
        } else {
            Insets ins = null;
            if (border instanceof DarkCellBorder) {
                ins = new Insets(0, 0, 0, 0);
            } else if (border != null) {
                ins = border.getBorderInsets(editor);
            }
            if (ins == null) ins = new Insets(0, 0, 0, 0);
            g.fillRect(ins.left, ins.top, editor.getWidth() - ins.left - ins.right,
                    editor.getHeight() - ins.top - ins.bottom);
        }
    }

    protected boolean isInCell(final JComponent c) {
        if (getBorder(c) instanceof DarkTextBorder) return false;
        return DarkUIUtil.getParentOfType(JSpinner.class, c, 2) != null || DarkUIUtil.isInCell(c)
                || PropertyUtil.getBooleanProperty(c, KEY_IS_TREE_EDITOR)
                || PropertyUtil.getBooleanProperty(c, KEY_IS_TABLE_EDITOR)
                || PropertyUtil.getBooleanProperty(c, KEY_IS_LIST_EDITOR);
    }

    protected Container getRelevantParent(final Component comp) {
        Container parent = comp.getParent();
        if (parent instanceof JSpinner.DefaultEditor) {
            JSpinner spinner = DarkUIUtil.getParentOfType(JSpinner.class, comp, 2);
            if (spinner != null) parent = spinner.getParent();
        } else if (parent instanceof JComboBox) {
            parent = parent.getParent();
        }
        return DarkUIUtil.getParentMatching(parent, c -> c.isOpaque() || c instanceof JTextComponent);
    }

    @Override
    protected void paintSafely(final Graphics g) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        super.paintSafely(g);
        config.restoreClip();
        paintDefaultText(g);
        config.restore();
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        StringPainter.paintOpacityBufferedUI(g, c, this);
    }

    @Override
    public void updateUI(final Graphics g, final JComponent c) {
        super.update(g, c);
    }

    protected void paintDefaultText(final Graphics g) {
        if (isEmpty()) {
            String defaultText = getDefaultText();
            if (!defaultText.isEmpty()) {
                Rectangle rect = getVisibleEditorRect();
                g.translate(rect.x, rect.y);
                Component renderer = getDefaultTextRenderer().getRendererComponent(editor, defaultText);
                renderer.setFont(editor.getFont());
                renderer.setBounds(rect);
                editor.add(renderer);
                renderer.paint(g);
                editor.remove(renderer);
            }
        }
    }

    protected String getDefaultText() {
        return PropertyUtil.getString(editor, KEY_DEFAULT_TEXT, "");
    }

    protected boolean isEmpty() {
        Document doc = editor.getDocument();
        return doc == null || doc.getLength() == 0;
    }

    protected DefaultTextRenderer getDefaultTextRenderer() {
        if (defaultTextRenderer == null) defaultTextRenderer = createDefaultTextRenderer();
        return defaultTextRenderer;
    }

    protected void paintBorderBackground(final Graphics2D g, final JTextComponent c, final DarkTextBorder border) {
        int arc = border.getArcSize(c);
        int bs = border.getBorderSize();
        PaintUtil.fillRoundRect(g, bs, bs, c.getWidth() - 2 * bs, c.getHeight() - 2 * bs, arc);
    }

    protected Border getBorder(final JComponent c) {
        return MarginBorderWrapper.getBorder(c);
    }

    protected Insets getBorderInsets(final JComponent c) {
        Border b = getBorder(c);
        if (b == null) {
            return new Insets(0, 0, 0, 0);
        }
        return b.getBorderInsets(c);
    }

    public Rectangle getDrawingRect(final JTextComponent c) {
        return DarkUIUtil.applyInsets(new Rectangle(c.getWidth(), c.getHeight()), getBorderInsets(c));
    }

    protected void installDarkKeyBoardActions() {
        ActionMap actionMap = SwingUtilities.getUIActionMap(getComponent());
        actionMap.put(TOGGLE_INSERT, new ToggleInsertAction());
        InputMap inputMap = SwingUtilities.getUIInputMap(getComponent(), JComponent.WHEN_FOCUSED);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, 0), TOGGLE_INSERT);
    }

    @Override
    public void installUI(final JComponent c) {
        if (c instanceof JTextComponent) {
            editor = (JTextComponent) c;
        }
        super.installUI(c);
        installDarkKeyBoardActions();
    }

    @Override
    protected Highlighter createHighlighter() {
        return new DarkHighlighter();
    }

    @Override
    protected Keymap createKeymap() {
        Keymap km = super.createKeymap();
        km.setDefaultAction(new DarkKeyTypedAction());
        return km;
    }

}
