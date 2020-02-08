/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.text;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.StyleSheet;
import java.awt.*;
import java.beans.PropertyChangeEvent;

/**
 * @author Jannis Weis
 */
public class DarkEditorPaneUI extends DarkTextUI {

    /**
     * Attribute key to reference the default font. used in javax.swing.text.StyleContext.getFont to resolve the default
     * font.
     */
    private static final String FONT_ATTRIBUTE_KEY = "FONT_ATTRIBUTE_KEY";


    /**
     * Creates a new BasicEditorPaneUI.
     */
    public DarkEditorPaneUI() {
        super();
    }

    /*
     * Implementation of DarkEditorPaneUI
     */

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkEditorPaneUI();
    }

    @Override
    protected DarkCaret.CaretStyle getDefaultCaretStyle() {
        return DarkCaret.CaretStyle.THICK_VERTICAL_LINE_STYLE;
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.5
     */
    public void installUI(final JComponent c) {
        super.installUI(c);
        updateDisplayProperties(c.getFont(), c.getForeground());
    }

    void updateDisplayProperties(final Font font, final Color fg) {
        JComponent c = getComponent();
        Object honorDisplayPropertiesObject = c.getClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES);
        boolean honorDisplayProperties = false;
        Object w3cLengthUnitsObject = c.getClientProperty(JEditorPane.
                                                                  W3C_LENGTH_UNITS);
        boolean w3cLengthUnits = false;
        if (honorDisplayPropertiesObject instanceof Boolean) {
            honorDisplayProperties = (Boolean) honorDisplayPropertiesObject;
        }
        if (w3cLengthUnitsObject instanceof Boolean) {
            w3cLengthUnits = (Boolean) w3cLengthUnitsObject;
        }
        if (this instanceof DarkTextPaneUI || honorDisplayProperties) {
            //using equals because can not use UIResource for Boolean
            Document doc = getComponent().getDocument();
            if (doc instanceof StyledDocument) {
                if (doc instanceof HTMLDocument && honorDisplayProperties) {
                    updateCSS(font, fg);
                } else {
                    updateStyle(font, fg);
                }
            }
        } else {
            cleanDisplayProperties();
        }
        if (w3cLengthUnits) {
            Document doc = getComponent().getDocument();
            if (doc instanceof HTMLDocument) {
                StyleSheet documentStyleSheet = ((HTMLDocument) doc).getStyleSheet();
                documentStyleSheet.addRule("W3C_LENGTH_UNITS_ENABLE");
            }
        } else {
            Document doc = getComponent().getDocument();
            if (doc instanceof HTMLDocument) {
                StyleSheet documentStyleSheet = ((HTMLDocument) doc).getStyleSheet();
                documentStyleSheet.addRule("W3C_LENGTH_UNITS_DISABLE");
            }

        }
    }

    private void updateCSS(final Font font, final Color fg) {
        JTextComponent component = getComponent();
        Document document = component.getDocument();
        if (document instanceof HTMLDocument) {
            StyleSheet styleSheet = new StyleSheetUIResource();
            StyleSheet documentStyleSheet =
                    ((HTMLDocument) document).getStyleSheet();
            StyleSheet[] styleSheets = documentStyleSheet.getStyleSheets();
            if (styleSheets != null) {
                for (StyleSheet s : styleSheets) {
                    if (s instanceof StyleSheetUIResource) {
                        documentStyleSheet.removeStyleSheet(s);
                    }
                }
            }
            String cssRule = sun.swing.SwingUtilities2.displayPropertiesToCSS(font, fg);
            styleSheet.addRule(cssRule);
            documentStyleSheet.addStyleSheet(styleSheet);
            documentStyleSheet.addRule("BASE_SIZE " +
                                               component.getFont().getSize());
            Style style = ((StyledDocument) document).getStyle(StyleContext.DEFAULT_STYLE);
            if (!font.equals(style.getAttribute(FONT_ATTRIBUTE_KEY))) {
                style.addAttribute(FONT_ATTRIBUTE_KEY, font);
            }
        }
    }

    private void updateStyle(final Font font, final Color fg) {
        updateFont(font);
        updateForeground(fg);
    }

    void cleanDisplayProperties() {
        Document document = getComponent().getDocument();
        if (document instanceof HTMLDocument) {
            StyleSheet documentStyleSheet = ((HTMLDocument) document).getStyleSheet();
            StyleSheet[] styleSheets = documentStyleSheet.getStyleSheets();
            if (styleSheets != null) {
                for (StyleSheet s : styleSheets) {
                    if (s instanceof StyleSheetUIResource) {
                        documentStyleSheet.removeStyleSheet(s);
                        documentStyleSheet.addRule("BASE_SIZE_DISABLE");
                        break;
                    }
                }
            }
            Style style = ((StyledDocument) document).getStyle(StyleContext.DEFAULT_STYLE);
            if (style.getAttribute(FONT_ATTRIBUTE_KEY) != null) {
                style.removeAttribute(FONT_ATTRIBUTE_KEY);
            }
        }
    }

    /**
     * Update the font in the default style of the document.
     *
     * @param font the new font to use or null to remove the font attribute from the document's style
     */
    private void updateFont(final Font font) {
        StyledDocument doc = (StyledDocument) getComponent().getDocument();
        Style style = doc.getStyle(StyleContext.DEFAULT_STYLE);

        if (style == null) {
            return;
        }

        String fontFamily = (String) style.getAttribute(StyleConstants.FontFamily);
        Integer fontSize = (Integer) style.getAttribute(StyleConstants.FontSize);
        Boolean isBold = (Boolean) style.getAttribute(StyleConstants.Bold);
        Boolean isItalic = (Boolean) style.getAttribute(StyleConstants.Italic);
        Font fontAttribute = (Font) style.getAttribute(FONT_ATTRIBUTE_KEY);
        if (font == null) {
            if (fontFamily != null) {
                style.removeAttribute(StyleConstants.FontFamily);
            }
            if (fontSize != null) {
                style.removeAttribute(StyleConstants.FontSize);
            }
            if (isBold != null) {
                style.removeAttribute(StyleConstants.Bold);
            }
            if (isItalic != null) {
                style.removeAttribute(StyleConstants.Italic);
            }
            if (fontAttribute != null) {
                style.removeAttribute(FONT_ATTRIBUTE_KEY);
            }
        } else {
            if (!font.getName().equals(fontFamily)) {
                StyleConstants.setFontFamily(style, font.getName());
            }
            if (fontSize == null || fontSize != font.getSize()) {
                StyleConstants.setFontSize(style, font.getSize());
            }
            if (isBold == null || isBold != font.isBold()) {
                StyleConstants.setBold(style, font.isBold());
            }
            if (isItalic == null || isItalic != font.isItalic()) {
                StyleConstants.setItalic(style, font.isItalic());
            }
            if (!font.equals(fontAttribute)) {
                style.addAttribute(FONT_ATTRIBUTE_KEY, font);
            }
        }
    }

    /**
     * Update the color in the default style of the document.
     *
     * @param color the new color to use or null to remove the color attribute from the document's style
     */
    private void updateForeground(final Color color) {
        StyledDocument doc = (StyledDocument) getComponent().getDocument();
        Style style = doc.getStyle(StyleContext.DEFAULT_STYLE);

        if (style == null) {
            return;
        }

        if (color == null) {
            if (style.getAttribute(StyleConstants.Foreground) != null) {
                style.removeAttribute(StyleConstants.Foreground);
            }
        } else {
            if (!color.equals(StyleConstants.getForeground(style))) {
                StyleConstants.setForeground(style, color);
            }
        }
    }

    /**
     * Fetch an action map to use.  The map for a JEditorPane is not shared because it changes with the EditorKit.
     */

    public ActionMap getActionMap() {
        ActionMap am = new ActionMapUIResource();
        am.put("requestFocus", new FocusAction());
        EditorKit editorKit = getEditorKit(getComponent());
        if (editorKit != null) {
            Action[] actions = editorKit.getActions();
            if (actions != null) {
                addActions(am, actions);
            }
        }
        am.put(TransferHandler.getCutAction().getValue(Action.NAME),
               TransferHandler.getCutAction());
        am.put(TransferHandler.getCopyAction().getValue(Action.NAME),
               TransferHandler.getCopyAction());
        am.put(TransferHandler.getPasteAction().getValue(Action.NAME),
               TransferHandler.getPasteAction());
        return am;
    }

    void addActions(final ActionMap map, final Action[] actions) {
        int n = actions.length;
        for (Action a : actions) {
            map.put(a.getValue(Action.NAME), a);
        }
    }

    /**
     * This method gets called when a bound property is changed on the associated JTextComponent.  This is a hook which
     * UI implementations may change to reflect how the UI displays bound properties of JTextComponent subclasses. This
     * is implemented to rebuild the ActionMap based upon an EditorKit change.
     *
     * @param evt the property change event
     */
    protected void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String name = evt.getPropertyName();
        if ("editorKit".equals(name)) {
            ActionMap map = SwingUtilities.getUIActionMap(getComponent());
            if (map != null) {
                Object oldValue = evt.getOldValue();
                if (oldValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit) oldValue).getActions();
                    if (actions != null) {
                        removeActions(map, actions);
                    }
                }
                Object newValue = evt.getNewValue();
                if (newValue instanceof EditorKit) {
                    Action[] actions = ((EditorKit) newValue).getActions();
                    if (actions != null) {
                        addActions(map, actions);
                    }
                }
            }
            updateFocusTraversalKeys();
        } else if ("editable".equals(name)) {
            updateFocusTraversalKeys();
        } else if ("foreground".equals(name)
                || "font".equals(name)
                || "document".equals(name)
                || JEditorPane.W3C_LENGTH_UNITS.equals(name)
                || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)
        ) {
            JComponent c = getComponent();
            updateDisplayProperties(c.getFont(), c.getForeground());
            if (JEditorPane.W3C_LENGTH_UNITS.equals(name)
                    || JEditorPane.HONOR_DISPLAY_PROPERTIES.equals(name)) {
                modelChanged();
            }
            if ("foreground".equals(name)) {
                Object honorDisplayPropertiesObject = c.getClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES);
                boolean honorDisplayProperties = false;
                if (honorDisplayPropertiesObject instanceof Boolean) {
                    honorDisplayProperties = (Boolean) honorDisplayPropertiesObject;
                }
                if (honorDisplayProperties) {
                    modelChanged();
                }
            }


        }
    }

    /**
     * Fetches the name used as a key to lookup properties through the UIManager.  This is used as a prefix to all the
     * standard text properties.
     *
     * @return the name ("EditorPane")
     */
    protected String getPropertyPrefix() {
        return "EditorPane";
    }

    /**
     * {@inheritDoc}
     *
     * @since 1.5
     */
    public void uninstallUI(final JComponent c) {
        cleanDisplayProperties();
        super.uninstallUI(c);
    }

    /**
     * Fetches the EditorKit for the UI.  This is whatever is currently set in the associated JEditorPane.
     *
     * @return the editor capabilities
     */
    public EditorKit getEditorKit(final JTextComponent tc) {
        JEditorPane pane = (JEditorPane) getComponent();
        return pane.getEditorKit();
    }

    void removeActions(final ActionMap map, final Action[] actions) {
        for (Action a : actions) {
            map.remove(a.getValue(Action.NAME));
        }
    }

    protected static class StyleSheetUIResource extends StyleSheet implements UIResource {
    }
}
