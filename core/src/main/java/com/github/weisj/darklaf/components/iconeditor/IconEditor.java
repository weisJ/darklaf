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
package com.github.weisj.darklaf.components.iconeditor;

import java.awt.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.stream.Collectors;

import javax.swing.*;

import com.github.weisj.darklaf.DynamicUI;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.components.CloseButton;
import com.github.weisj.darklaf.components.ComponentHelper;
import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.components.renderer.SimpleListCellRenderer;
import com.github.weisj.darklaf.icons.CustomThemedIcon;
import com.github.weisj.darklaf.icons.DerivableIcon;
import com.github.weisj.darklaf.icons.ThemedIcon;
import com.github.weisj.darklaf.icons.ThemedSVGIcon;
import com.github.weisj.darklaf.layout.HorizontalLayout;
import com.github.weisj.darklaf.layout.LayoutHelper;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.util.Actions;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.Pair;

public class IconEditor extends JPanel {

    private final Map<IconEditorPanel, JButton> editors = new HashMap<>();
    private final Map<Theme, Action> newEditorActions = new HashMap<>();
    private final Map<Theme, IconEditorPanel> editorMap = new HashMap<>();
    private final JComponent editorPanel;
    private final JComponent plusComp;
    private Icon selectedIcon;
    private boolean showEditorAddRemoveControls = true;

    public IconEditor(final List<Pair<String, ? extends Icon>> icons) {
        this(icons, 100);
    }

    public IconEditor(final List<Pair<String, ? extends Icon>> icons, final int displayIconSize) {
        setLayout(new BorderLayout());
        editorPanel = Box.createHorizontalBox();

        JComboBox<Pair<String, ThemedIcon>> iconCombo = new JComboBox<>();
        iconCombo.setMaximumSize(iconCombo.getPreferredSize());

        List<Pair<String, ThemedIcon>> themedIcons = icons.stream()
                .filter(p -> p.getSecond() instanceof ThemedIcon)
                .map(p -> new Pair<>(p.getFirst(), (ThemedIcon) p.getSecond()))
                .collect(Collectors.toList());

        ComboBoxModel<Pair<String, ThemedIcon>> model = new DefaultComboBoxModel<>(new Vector<>(themedIcons));
        iconCombo.setModel(model);
        iconCombo.setRenderer(SimpleListCellRenderer.create((c, p) -> {
            c.setText(p.getFirst());
            c.setIcon(p.getSecond());
        }));
        iconCombo.addItemListener(e -> {
            Pair<String, ThemedIcon> pair = model.getElementAt(iconCombo.getSelectedIndex());
            if (pair == null) return;
            ThemedIcon icon = model.getElementAt(iconCombo.getSelectedIndex()).getSecond();
            if (icon instanceof DerivableIcon) {
                icon = (ThemedIcon) ((DerivableIcon<?>) icon).derive(displayIconSize, displayIconSize);
            }
            selectedIcon = icon;
            ThemedIcon finalIcon = icon;
            editors.forEach((ed, cb) -> ed.setIcon(finalIcon));
        });
        iconCombo.setSelectedIndex(-1);

        Box north = Box.createHorizontalBox();
        north.setBorder(DarkBorders.createBottomBorderWithSpacing());
        north.add(Box.createHorizontalGlue());
        north.add(iconCombo);
        north.add(Box.createHorizontalGlue());

        plusComp = new JPanel(new GridBagLayout());
        int addIconSize = Math.min(Math.max(32, displayIconSize / 2), 50);
        JSplitButton addEditorButton = ComponentHelper.createIconOnlySplitButton(
                DarkUIUtil.ICON_LOADER.getIcon("navigation/add.svg", addIconSize, addIconSize, true));
        int overlaySize = (2 * addIconSize) / 3;
        addEditorButton.setOverlayDropDownIcon(
                DarkUIUtil.ICON_LOADER.getIcon("indicator/dropDown.svg", overlaySize, overlaySize, true));
        addEditorButton.setOverlayDropDownDisabledIcon(
                DarkUIUtil.ICON_LOADER.getIcon("indicator/dropDownDisabled.svg", overlaySize, overlaySize, true));
        addEditorButton.putClientProperty(ButtonConstants.KEY_ARC_MULTIPLIER, 3);
        JPopupMenu menu = addEditorButton.getActionMenu();

        for (Theme theme : LafManager.getRegisteredThemes()) {
            Action a = Actions.create(theme.getDisplayName(), e -> addEditor(theme, selectedIcon));
            newEditorActions.put(theme, a);
            menu.add(a);
        }
        plusComp.setBorder(LayoutHelper.createEmptyContainerBorder());
        plusComp.add(addEditorButton);
        plusComp.setVisible(showEditorAddRemoveControls);
        plusComp.setEnabled(showEditorAddRemoveControls);

        JComponent holder = new JPanel(new HorizontalLayout());
        holder.add(editorPanel);
        holder.add(plusComp);

        OverlayScrollPane sp = new OverlayScrollPane(holder);
        sp.setAddVerticalScrollBarSize(true);

        add(north, BorderLayout.NORTH);
        add(sp, BorderLayout.CENTER);
        addEditor(LafManager.getTheme());
        iconCombo.setSelectedIndex(0);
    }

    /**
     * Adds an editor with the given theme. If duplicate editors aren't allowed and there already is an
     * editor with the given theme nothing will happen and {@code null} is returned.
     *
     * @see #removeEditor(IconEditorPanel)
     * @param theme the theme of the editor.
     * @return the created editor.
     */
    public IconEditorPanel addEditor(final Theme theme) {
        return addEditor(theme, selectedIcon);
    }

    private IconEditorPanel addEditor(final Theme theme, final Icon icon) {
        Theme t = Theme.baseThemeOf(theme);
        IconEditorPanel prevEditor = editorMap.get(t);
        if (prevEditor != null) return prevEditor;

        IconEditorPanel editor = new IconEditorPanel(icon, theme);
        editor.setBorder(DarkBorders.createRightBorder());
        newEditorActions.get(t).setEnabled(false);
        editorMap.put(t, editor);

        JButton closeButton = new CloseButton();
        DynamicUI.withLocalizedTooltip(closeButton, "Actions.close");
        closeButton.setIcon(changeIconTheme((ThemedSVGIcon) closeButton.getIcon(), theme));
        closeButton.setDisabledIcon(changeIconTheme((ThemedSVGIcon) closeButton.getDisabledIcon(), theme));
        closeButton.setRolloverIcon(changeIconTheme((ThemedSVGIcon) closeButton.getRolloverIcon(), theme));
        closeButton.addActionListener(e -> removeEditor(editor));
        closeButton.setEnabled(showEditorAddRemoveControls);
        closeButton.setVisible(showEditorAddRemoveControls);

        if (newEditorActions.size() == editorMap.size()) {
            plusComp.setEnabled(false);
            plusComp.setVisible(false);
        }

        JComponent editorHolder = LayoutHelper.createPanelWithOverlay(editor, closeButton, Alignment.NORTH_WEST,
                LayoutHelper.createEmptyContainerInsets());

        editors.put(editor, closeButton);
        editorPanel.add(editorHolder);
        updateLayout();
        return editor;
    }

    private Icon changeIconTheme(final ThemedSVGIcon icon, final Theme theme) {
        if (icon == null) return null;
        return new CustomThemedIcon(icon,
                IconEditorPanel.IconValues.getIconDefaults(theme).getDefaults(),
                CustomThemedIcon.MergeMode.REMOVE_REFERENCES);
    }

    /**
     * Remove a given editor.
     *
     * @param editor the editor to remove.
     */
    public void removeEditor(final IconEditorPanel editor) {
        if (editors.remove(editor) == null) return;
        Theme t = Theme.baseThemeOf(editor.getTheme());
        editorMap.remove(t);
        newEditorActions.get(t).setEnabled(true);
        plusComp.setVisible(true);
        plusComp.setEnabled(true);
        editorPanel.remove(editor.getParent());
        updateLayout();
    }

    private void updateLayout() {
        editorPanel.getParent().doLayout();
        editorPanel.getParent().repaint();
    }

    /**
     * Returns whether the list of editors can be modified through visual buttons.
     *
     * @return true of add/remove buttons are displayed.
     */
    public boolean isShowEditorAddRemoveControls() {
        return showEditorAddRemoveControls;
    }

    /**
     * Sets whether the list of editors can be modified through visual buttons.
     *
     * @param showEditorAddRemoveControls true of add/remove buttons are displayed.
     */
    public void setShowEditorAddRemoveControls(final boolean showEditorAddRemoveControls) {
        if (this.showEditorAddRemoveControls != showEditorAddRemoveControls) {
            if (!showEditorAddRemoveControls) {
                plusComp.setVisible(false);
                plusComp.setEnabled(false);
            } else if (editorMap.size() < newEditorActions.size()) {
                plusComp.setVisible(true);
                plusComp.setEnabled(true);
            }
            editors.forEach((k, b) -> {
                b.setVisible(showEditorAddRemoveControls);
                b.setEnabled(showEditorAddRemoveControls);
            });
            updateLayout();
        }
        this.showEditorAddRemoveControls = showEditorAddRemoveControls;
    }

    /**
     * Export the properties of all editors.
     *
     * @return all editor properties.
     */
    public List<Pair<Theme, Map<String, Object>>> exportProperties() {
        return editors.keySet().stream()
                .map(e -> new Pair<>(e.getTheme(), e.exportProperties()))
                .collect(Collectors.toList());
    }
}
