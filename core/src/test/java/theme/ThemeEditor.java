/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package theme;

import java.awt.*;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import ui.ComponentDemo;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.DefaultButton;
import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.graphics.ThemedColor;
import com.github.weisj.darklaf.icons.DerivableIcon;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.OverlayIcon;
import com.github.weisj.darklaf.icons.TextIcon;
import com.github.weisj.darklaf.layout.LayoutManagerAdapter;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.ThemeDelegate;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.theme.info.ColorToneRule;
import com.github.weisj.darklaf.theme.info.ContrastRule;
import com.github.weisj.darklaf.theme.info.FontSizeRule;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.ui.table.TableConstants;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.FontUtil;
import defaults.UIManagerDefaults;

public class ThemeEditor extends JPanel {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install(ComponentDemo.getTheme().derive(
                    FontSizeRule.getDefault(),
                    AccentColorRule.getDefault()));
            JFrame frame = new JFrame("Theme Editor");
            frame.setContentPane(new ThemeEditor());
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }

    public ThemeEditor() {
        setLayout(new BorderLayout());

        AtomicReference<Theme> baseTheme = new AtomicReference<>(LafManager.getInstalledTheme());

        LinkedHashMap<Object, Object> themeDefaults = new LinkedHashMap<>();
        LinkedHashMap<Object, Object> iconDefaults = new LinkedHashMap<>();
        LinkedHashMap<Object, Object> globalDefaults = new LinkedHashMap<>();
        LinkedHashMap<Object, Object> platformDefaults = new LinkedHashMap<>();
        LinkedHashMap<Object, Object> uiDefaults = new LinkedHashMap<>();

        JComboBox<Theme> themeCombo = new JComboBox<>(LafManager.getThemeComboBoxModel());
        themeCombo.setMaximumSize(themeCombo.getPreferredSize());
        themeCombo.setSelectedItem(baseTheme.get());

        JButton setBaseTheme = new JButton("Set");

        JComponent themeArea = Box.createHorizontalBox();
        themeArea.setBorder(BorderFactory.createCompoundBorder(DarkBorders.createLineBorder(0, 0, 1, 0),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
        themeArea.add(new JLabel("Base Theme: "));
        themeArea.add(themeCombo);
        themeArea.add(Box.createHorizontalStrut(5));
        themeArea.add(setBaseTheme);
        themeArea.add(Box.createHorizontalGlue());
        add(themeArea, BorderLayout.NORTH);

        JPanel content = new JPanel(new BorderLayout());

        JToggleButton darkToggle = new JToggleButton("Light/Dark");
        darkToggle.putClientProperty(ToggleButtonConstants.KEY_VARIANT, ToggleButtonConstants.VARIANT_SLIDER);
        darkToggle.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        JToggleButton contrastToggle = new JToggleButton("Low/High Contrast");
        contrastToggle.putClientProperty(ToggleButtonConstants.KEY_VARIANT, ToggleButtonConstants.VARIANT_SLIDER);
        contrastToggle.setAlignmentX(JComponent.LEFT_ALIGNMENT);

        JComponent settingsArea = Box.createVerticalBox();
        settingsArea.setBorder(BorderFactory.createCompoundBorder(DarkBorders.createLineBorder(0, 0, 1, 0),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
        settingsArea.add(Box.createVerticalStrut(5));
        settingsArea.add(darkToggle);
        settingsArea.add(Box.createVerticalStrut(5));
        settingsArea.add(contrastToggle);
        settingsArea.add(Box.createVerticalStrut(5));

        content.add(settingsArea, BorderLayout.NORTH);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        tabbedPane.addTab("Theme defaults", createTable(themeDefaults, false));
        tabbedPane.addTab("Icon defaults", createTable(iconDefaults, false));
        tabbedPane.addTab("UI customs", createTable(uiDefaults, true));
        tabbedPane.addTab("Global customs", createTable(globalDefaults, true));
        tabbedPane.addTab("Platform customs", createTable(platformDefaults, true));
        tabbedPane.addTab("All Defaults (Read only)", new UIManagerDefaults().createComponent());

        content.add(tabbedPane, BorderLayout.CENTER);

        add(content, BorderLayout.CENTER);


        JComponent buttonArea = Box.createHorizontalBox();
        buttonArea.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        buttonArea.add(Box.createHorizontalGlue());


        JButton apply = new DefaultButton("Apply");
        buttonArea.add(apply);
        add(buttonArea, BorderLayout.SOUTH);

        MutableTheme mutableTheme = new MutableTheme(baseTheme.get()) {

            @Override
            public void loadDefaults(final Properties properties, final UIDefaults currentDefaults) {
                super.loadDefaults(properties, currentDefaults);
                putAll(properties, themeDefaults);
                properties.put(KEY_DARK, darkToggle.isSelected());
                properties.put(KEY_HIGH_CONTRAST, contrastToggle.isSelected());
            }

            @Override
            public void customizeGlobals(final Properties properties, final UIDefaults currentDefaults) {
                properties.putAll(globalDefaults);
            }

            @Override
            public void customizePlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
                properties.putAll(platformDefaults);
            }

            @Override
            public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults) {
                properties.putAll(uiDefaults);
            }

            @Override
            public void customizeIconTheme(final Properties properties, final UIDefaults currentDefaults) {
                properties.putAll(iconDefaults);
            }
        };
        AtomicBoolean updating = new AtomicBoolean();

        apply.addActionListener(e -> {
            mutableTheme.setDelegate(baseTheme.get());
            mutableTheme.colorToneRule =
                    darkToggle.isSelected() ? ColorToneRule.DARK : ColorToneRule.LIGHT;
            mutableTheme.contrastRule =
                    contrastToggle.isSelected() ? ContrastRule.HIGH_CONTRAST : ContrastRule.STANDARD;
            LafManager.setTheme((Theme) null);
            LafManager.install(mutableTheme);
        });

        setBaseTheme.addActionListener(e -> {
            Theme t = new ThemeDelegate((Theme) themeCombo.getSelectedItem()) {

                @Override
                public void customizeGlobals(final Properties properties, final UIDefaults currentDefaults) {
                    RecordingProperties props = new RecordingProperties(properties);
                    super.customizeGlobals(props, currentDefaults);
                    putAll(globalDefaults, props.getRecording());
                }

                @Override
                public void customizePlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
                    RecordingProperties props = new RecordingProperties(properties);
                    super.customizePlatformProperties(props, currentDefaults);
                    putAll(platformDefaults, props.getRecording());
                }

                @Override
                public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults) {
                    RecordingProperties props = new RecordingProperties(properties);
                    super.customizeUIProperties(props, currentDefaults);
                    putAll(uiDefaults, props.getRecording());
                }
            };
            baseTheme.set(t);
            updating.set(true);
            MutableThemedLaf themedLaf = new MutableThemedLaf();
            themedLaf.setTheme(t);
            LafManager.setTheme(t);
            UIDefaults defaults = themedLaf.getDefaults();

            for (String key : THEME_KEYS) {
                themeDefaults.put(key, defaults.get(key));
            }

            for (String key : ICON_KEYS) {
                iconDefaults.put(key, defaults.get(key));
            }

            darkToggle.setSelected(Theme.isDark(t));
            contrastToggle.setSelected(Theme.isHighContrast(t));

            tabbedPane.repaint();
            updating.set(false);
        });
        setBaseTheme.doClick();
    }

    private void putAll(final Map<Object, Object> target, final Map<Object, Object> values) {
        values.forEach((k, v) -> {
            if (v instanceof Color) {
                v = new DarkColorUIResource((Color) v);
            }
            target.put(k, v);
        });
    }

    enum EntryType {
        COLOR("Add Color", Color.BLACK, DarkUIUtil.ICON_LOADER.getIcon("menu/colorChooser.svg")),
        INTEGER("Add Integer", 0, ((Supplier<Icon>) () -> {
            Font font = FontUtil.createFont(Font.MONOSPACED, Font.BOLD, 13);
            return new TextIcon("42", new ThemedColor("menuIconEnabled"), font, 16, 16);
        }).get()),
        BOOLEAN("Add Boolean", false, DarkUIUtil.ICON_LOADER.getIcon("control/checkboxSelectedFocused.svg", true)),
        STRING("Add String", "", IconLoader.get().getIcon("icon/word.svg", true));

        private final String s;
        private final Object defaultValue;
        private final Icon icon;
        private int entryCount;

        EntryType(final String s, final Object defaultValue, final Icon icon) {
            this.s = s;
            this.defaultValue = defaultValue;
            this.icon = icon;
        }

        public String getText() {
            return s;
        }

        @SuppressWarnings("unchecked")
        public DerivableIcon<Icon> getIcon() {
            return (DerivableIcon<Icon>) icon;
        }

        public void addElement(final JTable table) {
            DefaultTableModel model = (DefaultTableModel) table.getModel();
            model.addRow(new Object[] {"<" + name() + "_" + (entryCount++) + ">", defaultValue});
        }
    }

    private JComponent createTable(final LinkedHashMap<Object, Object> valueMap, final boolean keyEditable) {
        JTable table = new JTable();
        table.getTableHeader().setReorderingAllowed(false);
        table.setShowHorizontalLines(false);
        table.setModel(new MapTableModel(valueMap, keyEditable));
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.putClientProperty(TableConstants.KEY_CELL_VALUE_DETERMINES_EDITOR_CLASS, true);

        JPanel buttonPanel = keyEditable ? createButtonPanel(table) : null;

        OverlayScrollPane sp = new OverlayScrollPane(table) {
            @Override
            public void doLayout() {
                super.doLayout();
                if (!keyEditable) return;
                Dimension prefSize = buttonPanel.getPreferredSize();
                int pad = 10;
                Component viewport = getScrollPane().getViewport();
                Rectangle viewBounds = new Rectangle(0, 0, viewport.getWidth(), viewport.getHeight());
                viewBounds = SwingUtilities.convertRectangle(viewport, viewBounds, this);
                int x = viewBounds.x - pad - prefSize.width;
                int columnEnd = table.getColumnModel().getColumn(0).getWidth();
                if (getVerticalScrollBar() != null && getVerticalScrollBar().isVisible()) {
                    columnEnd = Math.min(columnEnd, viewBounds.width - getVerticalScrollBar().getWidth());
                }
                x += columnEnd;
                buttonPanel.setBounds(x, viewBounds.y + pad, prefSize.width, prefSize.height);
                buttonPanel.doLayout();
            }
        };
        if (keyEditable) {
            sp.add(buttonPanel, Integer.valueOf(JLayeredPane.MODAL_LAYER - 1));
        }
        sp.doLayout();
        sp.getScrollPane().setBorder(DarkBorders.createLineBorder(0, 0, 1, 0));

        return sp;
    }

    private JPanel createButtonPanel(final JTable table) {
        AtomicReference<EntryType> lastSelection = new AtomicReference<>(EntryType.COLOR);

        OverlayIcon overlayIcon =
                new OverlayIcon(
                        DarkUIUtil.ICON_LOADER.getIcon("navigation/add.svg", true),
                        lastSelection.get().getIcon().derive(8, 8),
                        Alignment.SOUTH_EAST);
        JSplitButton addButton = new JSplitButton(overlayIcon);
        addButton.setFocusable(false);
        addButton.setToolTipText(lastSelection.get().getText());

        JPopupMenu menu = addButton.getActionMenu();
        for (EntryType type : EntryType.values()) {
            menu.add(new JMenuItem(type.getText(), type.getIcon())).addActionListener(e -> {
                lastSelection.set(type);
                addButton.setToolTipText(type.getText());
                overlayIcon.setOverlay(type.getIcon().derive(8, 8));
                addButton.repaint();
            });
        }
        addButton.addActionListener(e -> lastSelection.get().addElement(table));
        addButton.putClientProperty(ButtonConstants.KEY_VARIANT, ButtonConstants.VARIANT_BORDERLESS);
        addButton.putClientProperty(ButtonConstants.KEY_SQUARE, true);
        addButton.putClientProperty(ButtonConstants.KEY_THIN, true);

        JButton deleteButton = new JButton();
        deleteButton.setToolTipText("Remove selected entry");
        deleteButton.setIcon(DarkUIUtil.ICON_LOADER.getIcon("menu/delete.svg"));
        deleteButton.setDisabledIcon(DarkUIUtil.ICON_LOADER.getIcon("menu/deleteDisabled.svg"));
        deleteButton.putClientProperty(ButtonConstants.KEY_VARIANT, ButtonConstants.VARIANT_BORDERLESS);
        deleteButton.putClientProperty(ButtonConstants.KEY_SQUARE, true);
        deleteButton.putClientProperty(ButtonConstants.KEY_THIN, true);
        deleteButton.setEnabled(table.getSelectedRow() >= 0);
        deleteButton.setFocusable(false);

        deleteButton.addActionListener(e -> {
            int row = table.getSelectedRow();
            ((DefaultTableModel) table.getModel()).removeRow(row);
            table.clearSelection();
            int newRow = row > 0 ? row - 1 : row;
            if (newRow < table.getModel().getRowCount()) {
                table.addRowSelectionInterval(newRow, newRow);
            }
        });
        table.getSelectionModel().addListSelectionListener(e -> deleteButton.setEnabled(table.getSelectedRow() >= 0));

        JPanel buttonPanel = new JPanel() {
            @Override
            protected void paintComponent(final Graphics g) {
                super.paintComponent(g);
                GraphicsContext context = new GraphicsContext(g);
                ((Graphics2D) g).setComposite(PaintUtil.getGlowComposite());
                g.setColor(UIManager.getColor("hoverHighlight"));
                PaintUtil.fillRoundRect((Graphics2D) g, 0, 0, getWidth(), getHeight(), 10, false);
                context.restore();
            }
        };
        buttonPanel.setLayout(new LayoutManagerAdapter() {

            private final int pad = 3;

            @Override
            public void layoutContainer(final Container parent) {
                Dimension addPref = addButton.getPreferredSize();
                Dimension delPref = deleteButton.getPreferredSize();
                addButton.setBounds(pad, pad, addPref.width, addPref.height);
                deleteButton.setBounds(2 * pad + addPref.width, pad, delPref.width, delPref.height);
            }

            @Override
            public Dimension preferredLayoutSize(final Container parent) {
                Dimension dim = addButton.getPreferredSize();
                Dimension deleteSize = deleteButton.getPreferredSize();
                dim.width += deleteSize.width + pad;
                dim.height = Math.max(deleteSize.height, dim.height);
                dim.width += 2 * pad;
                dim.height += 2 * pad;
                return dim;
            }

            @Override
            public Dimension minimumLayoutSize(final Container parent) {
                return preferredLayoutSize(parent);
            }
        });
        buttonPanel.add(addButton);
        buttonPanel.add(deleteButton);
        buttonPanel.setOpaque(false);
        return buttonPanel;
    }

    private static class MutableThemedLaf extends DarkLaf {

        @Override
        public void setTheme(final Theme theme) {
            super.setTheme(theme);
        }
    }

    private static class MutableTheme extends ThemeDelegate {

        private Theme delegate;
        private ColorToneRule colorToneRule;
        private ContrastRule contrastRule;

        public MutableTheme(final Theme delegate) {
            super(delegate);
            setDelegate(delegate);
        }

        public void setDelegate(final Theme delegate) {
            this.delegate = delegate;
            colorToneRule = delegate.getColorToneRule();
            contrastRule = delegate.getContrastRule();
        }

        @Override
        public Theme getDelegate() {
            return delegate;
        }

        @Override
        public ContrastRule getContrastRule() {
            return contrastRule;
        }

        @Override
        public ColorToneRule getColorToneRule() {
            return colorToneRule;
        }
    }

    private static final String KEY_DARK = "Theme.dark";
    private static final String KEY_HIGH_CONTRAST = "Theme.highContrast";
    private static final String[] THEME_KEYS = {
            "background",
            "backgroundAlternative",
            "backgroundColorful",
            "backgroundColorfulInactive",
            "backgroundContainer",
            "backgroundHeader",
            "backgroundToolTip",
            "backgroundToolTipInactive",
            "backgroundHover",
            "backgroundSelected",
            "backgroundHoverSecondary",
            "backgroundSelectedSecondary",
            "backgroundHoverColorful",
            "backgroundSelectedColorful",
            "dropBackground",
            "dropForeground",
            "borderSecondary",
            "border",
            "borderTertiary",
            "borderFocus",
            "gridLine",
            "hoverHighlight",
            "clickHighlight",
            "hoverHighlightOutline",
            "clickHighlightOutline",
            "hoverHighlightColorful",
            "clickHighlightColorful",
            "hoverHighlightDefault",
            "clickHighlightDefault",
            "hoverHighlightSecondary",
            "clickHighlightSecondary",
            "highlightFill",
            "highlightFillFocus",
            "highlightFillFocusSecondary",
            "highlightFillMono",
            "widgetBorder",
            "widgetBorderInactive",
            "widgetBorderDefault",
            "widgetFill",
            "widgetFillSelected",
            "widgetFillInactive",
            "widgetFillDefault",
            "controlBorder",
            "controlBorderDisabled",
            "controlBorderSelected",
            "controlBorderFocus",
            "controlBorderFocusSelected",
            "controlBorderSecondary",
            "controlFill",
            "controlFillFocus",
            "controlFillSecondary",
            "controlTrack",
            "controlFillDisabled",
            "controlFillHighlight",
            "controlFillHighlightDisabled",
            "controlBackground",
            "controlFadeStart",
            "controlFadeEnd",
            "controlFadeStartSecondary",
            "controlFadeEndSecondary",
            "controlErrorFadeStart",
            "controlErrorFadeEnd",
            "controlPassedFadeStart",
            "controlPassedFadeEnd",
            "caret",
            "textForeground",
            "textForegroundDefault",
            "textForegroundHighlight",
            "textForegroundInactive",
            "textForegroundSecondary",
            "acceleratorForeground",
            "textContrastForeground",
            "textSelectionForeground",
            "textSelectionForegroundInactive",
            "textSelectionForegroundDisabled",
            "textSelectionBackground",
            "textSelectionBackgroundSecondary",
            "textBackground",
            "textBackgroundInactive",
            "textBackgroundSecondary",
            "textBackgroundSecondaryInactive",
            "textCompSelectionForeground",
            "textCompSelectionBackground",
            "hyperlink",
            "shadow",
            "glowOpacity",
            "dropOpacity",
            "shadowOpacityLight",
            "shadowOpacityStrong",
            "glowFocus",
            "glowFocusInactive",
            "glowFocusLine",
            "glowFocusLineInactive",
            "glowError",
            "glowErrorLine",
            "glowFocusError",
            "glowFocusErrorLine",
            "glowWarning",
            "glowWarningLine",
            "arc",
            "arcFocus",
            "arcSecondary",
            "arcSecondaryFocus",
            "borderThickness",
            "shadowHeight"
    };
    private static final String[] ICON_KEYS = {
            "menuIconOpacity",
            "navigationIconOpacity",
            "fileIconOpacity",
            "menuIconEnabled",
            "menuIconHovered",
            "menuIconSelected",
            "menuIconSelectedSecondary",
            "menuIconDisabled",
            "menuIconHighlight",
            "fileIconBackground",
            "fileIconForeground",
            "fileIconHighlight",
            "windowButton",
            "windowButtonDisabled",
            "windowCloseHovered",
            "errorIconColor",
            "informationIconColor",
            "warningIconColor",
            "questionIconColor"
    };
}
