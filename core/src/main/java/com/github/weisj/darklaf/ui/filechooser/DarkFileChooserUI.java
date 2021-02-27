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
package com.github.weisj.darklaf.ui.filechooser;

import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.filechooser.FileSystemView;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.ComponentUI;

import sun.swing.FilePane;

import com.github.weisj.darklaf.components.tooltip.TooltipAwareButton;
import com.github.weisj.darklaf.components.tooltip.TooltipAwareToggleButton;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.combobox.ComboBoxConstants;
import com.github.weisj.darklaf.util.AlignmentExt;

public class DarkFileChooserUI extends DarkFileChooserUIBridge {

    public static final String KEY_VIEW_TYPE = "viewType";
    private static final String MIME_TEXT = "text/";
    private static final String MIME_IMAGE = "image/";

    public DarkFileChooserUI(final JFileChooser b) {
        super(b);
    }

    protected AlignedLabel filesOfTypeLabel;
    protected BasicFileView fileView;
    protected Icon textFileIcon;
    protected Icon imageFileIcon;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkFileChooserUI((JFileChooser) c);
    }

    @Override
    protected void installIcons(final JFileChooser fc) {
        super.installIcons(fc);
        textFileIcon = UIManager.getIcon("FileView.textFileIcon");
        imageFileIcon = UIManager.getIcon("FileView.imageFileIcon");
    }

    @Override
    protected void uninstallIcons(final JFileChooser fc) {
        super.uninstallIcons(fc);
        textFileIcon = null;
        imageFileIcon = null;
    }

    @Override
    public void installComponents(final JFileChooser fc) {
        fc.setBorder(new EmptyBorder(10, 10, 7, 10));
        fc.setLayout(new BorderLayout(0, 7));
        filePane = createFilePane(fc);

        fc.add(createTopPanel(fc), BorderLayout.NORTH);
        fc.add(filePane, BorderLayout.CENTER);
        fc.add(createBottomPanel(fc), BorderLayout.SOUTH);
        fc.add(createControlPanel(fc), BorderLayout.AFTER_LINE_ENDS);

        setupButtonPanel(fc);
        if (fc.getControlButtonsAreShown()) {
            addControlButtons();
        }
        groupLabels(new AlignedLabel[] {fileNameLabel, filesOfTypeLabel});
    }

    protected void setupButtonPanel(final JFileChooser fileChooser) {
        JComponent buttonPanel = getButtonPanel();
        buttonPanel.setLayout(new DarkButtonAreaLayout());

        approveButton = new TooltipAwareButton(getApproveButtonText(fileChooser));
        approveButton.putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
        approveButton.addActionListener(getApproveSelectionAction());
        approveButton.setToolTipText(getApproveButtonToolTipText(fileChooser));

        cancelButton = new TooltipAwareButton(cancelButtonText);
        cancelButton.putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
        cancelButton.setToolTipText(cancelButtonToolTipText);
        cancelButton.addActionListener(getCancelSelectionAction());

        buttonPanel.add(approveButton);
        buttonPanel.add(cancelButton);
    }

    protected JComponent createControlPanel(final JFileChooser fileChooser) {
        JComponent accessoryPanel = getAccessoryPanel();
        JComponent accessory = fileChooser.getAccessory();
        if (accessory != null) accessoryPanel.add(accessory);
        return accessoryPanel;
    }

    protected DarkFilePaneUIBridge createFilePane(final JFileChooser fileChooser) {
        DarkFilePaneUIBridge filePane = new DarkFilePane(new MetalFileChooserUIAccessor());
        fileChooser.addPropertyChangeListener(filePane);

        // ComboBox Label
        filePane.addPropertyChangeListener(e -> {
            if (KEY_VIEW_TYPE.equals(e.getPropertyName())) {
                int viewType = filePane.getViewType();
                switch (viewType) {
                    case FilePane.VIEWTYPE_LIST:
                        listViewButton.setSelected(true);
                        break;

                    case FilePane.VIEWTYPE_DETAILS:
                        detailsViewButton.setSelected(true);
                        break;
                }
            }
        });
        filePane.setPreferredSize(LIST_PREF_SIZE);
        return filePane;
    }

    protected JComponent createBottomPanel(final JFileChooser fileChooser) {
        JPanel bottomPanel = getBottomPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        bottomPanel.add(createFileNamePanel(fileChooser));
        bottomPanel.add(makeVerticalSpacer());
        bottomPanel.add(createFileTypePanel(fileChooser));
        return bottomPanel;
    }

    protected JComponent createFileNamePanel(final JFileChooser fileChooser) {
        JPanel fileNamePanel = new JPanel();
        fileNamePanel.setLayout(new BoxLayout(fileNamePanel, BoxLayout.LINE_AXIS));
        fileNameLabel = new AlignedLabel();
        populateFileNameLabel();
        fileNamePanel.add(fileNameLabel);
        fileNameTextField = new FileTextField();
        fileNamePanel.add(fileNameTextField);
        fileNameLabel.setLabelFor(fileNameTextField);
        fileNameTextField.addFocusListener(new FocusAdapter() {
            public void focusGained(final FocusEvent e) {
                if (!getFileChooser().isMultiSelectionEnabled()) {
                    filePane.clearSelection();
                }
            }
        });
        if (fileChooser.isMultiSelectionEnabled()) {
            setFileName(fileNameString(fileChooser.getSelectedFiles()));
        } else {
            setFileName(fileNameString(fileChooser.getSelectedFile()));
        }
        return fileNamePanel;
    }

    protected Component createFileTypePanel(final JFileChooser fileChooser) {
        JPanel filesOfTypePanel = new JPanel();
        filesOfTypePanel.setLayout(new BoxLayout(filesOfTypePanel, BoxLayout.LINE_AXIS));

        filesOfTypeLabel = new AlignedLabel(filesOfTypeLabelText);
        filesOfTypeLabel.setDisplayedMnemonic(filesOfTypeLabelMnemonic);
        filesOfTypePanel.add(filesOfTypeLabel);

        filterComboBoxModel = createFilterComboBoxModel();
        fileChooser.addPropertyChangeListener(filterComboBoxModel);
        filterComboBox = createFilterComboBox(filterComboBoxModel);
        filesOfTypeLabel.setLabelFor(filterComboBox);
        filesOfTypePanel.add(filterComboBox);

        return filesOfTypePanel;
    }

    protected JComboBox<Object> createFilterComboBox(final ComboBoxModel<Object> model) {
        JComboBox<Object> comboBox = new JComboBox<>(model);
        model.addListDataListener(new ListDataListener() {
            @Override
            public void intervalAdded(final ListDataEvent e) {
                comboBox.setEnabled(comboBox.getItemCount() > 1);
            }

            @Override
            public void intervalRemoved(final ListDataEvent e) {
                comboBox.setEnabled(comboBox.getItemCount() > 1);
            }

            @Override
            public void contentsChanged(final ListDataEvent e) {
                comboBox.setEnabled(comboBox.getItemCount() > 1);
            }
        });
        comboBox.putClientProperty(ComboBoxConstants.KEY_DO_NOT_UPDATE_WHEN_SCROLLED, true);
        comboBox.putClientProperty(AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY, filesOfTypeLabelText);
        comboBox.setRenderer(createFilterComboBoxRenderer());
        return comboBox;
    }

    protected JComponent createTopPanel(final JFileChooser fileChooser) {
        JPanel topPanel = new JPanel(new BorderLayout(10, 0));
        lookInLabel = new JLabel(lookInLabelText);
        lookInLabel.setDisplayedMnemonic(lookInLabelMnemonic);

        directoryComboBox = createDirectoryComboBox(fileChooser);
        lookInLabel.setLabelFor(directoryComboBox);
        directoryComboBoxModel = createDirectoryComboBoxModel(fileChooser);
        directoryComboBox.setModel(directoryComboBoxModel);

        topPanel.add(lookInLabel, BorderLayout.BEFORE_LINE_BEGINS);
        topPanel.add(directoryComboBox, BorderLayout.CENTER);
        topPanel.add(createTopButtonArea(), BorderLayout.AFTER_LINE_ENDS);
        return topPanel;
    }

    private JComboBox<Object> createDirectoryComboBox(final JFileChooser fileChooser) {
        JComboBox<Object> comboBox = new JComboBox<Object>() {
            public Dimension getPreferredSize() {
                Dimension d = super.getPreferredSize();
                // Must be small enough to not affect total width.
                d.width = 150;
                return d;
            }
        };
        comboBox.putClientProperty(ComboBoxConstants.KEY_DO_NOT_UPDATE_WHEN_SCROLLED, true);
        comboBox.putClientProperty(AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY, lookInLabelText);
        comboBox.addActionListener(directoryComboBoxAction);
        comboBox.setRenderer(createDirectoryComboBoxRenderer(fileChooser));
        comboBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        comboBox.setAlignmentY(JComponent.TOP_ALIGNMENT);
        comboBox.setMaximumRowCount(8);
        return comboBox;
    }

    protected Component createTopButtonArea() {
        Box box = Box.createHorizontalBox();

        box.add(createUpFolderButton());
        box.add(makeHorizontalSpacer());

        box.add(createHomeButton());
        box.add(makeHorizontalSpacer());

        if (!UIManager.getBoolean("FileChooser.readOnly")) {
            box.add(createNewDirectoryButton());
            box.add(makeHorizontalSpacer());
        }
        box.add(createViewButtonArea());
        box.add(Box.createGlue());
        return box;
    }

    protected Component makeHorizontalSpacer() {
        return Box.createRigidArea(hstrut5);
    }

    protected Component makeVerticalSpacer() {
        return Box.createRigidArea(vstrut5);
    }

    protected JComponent createViewButtonArea() {
        Box box = Box.createHorizontalBox();
        listViewButton = createListViewButton();
        detailsViewButton = createDetailsViewButton();
        listViewButton.putClientProperty(DarkButtonUI.KEY_RIGHT_NEIGHBOUR, detailsViewButton);
        detailsViewButton.putClientProperty(DarkButtonUI.KEY_LEFT_NEIGHBOUR, listViewButton);

        box.add(listViewButton);
        box.add(detailsViewButton);

        listViewButton.addItemListener(e -> detailsViewButton.setSelected(!listViewButton.isSelected()));
        detailsViewButton.addItemListener(e -> listViewButton.setSelected(!detailsViewButton.isSelected()));

        return box;
    }

    protected JButton createNewDirectoryButton() {
        JButton b = new TooltipAwareButton(filePane.getNewFolderAction());
        setupButton(b, newFolderAccessibleName, newFolderToolTipText);
        b.setText(null);
        b.setIcon(newFolderIcon);
        return b;
    }

    protected JButton createHomeButton() {
        JButton b = new TooltipAwareButton(homeFolderIcon);
        setupButton(b, homeFolderAccessibleName, homeFolderToolTipText);
        b.addActionListener(getGoHomeAction());
        return b;
    }

    protected JButton createUpFolderButton() {
        JButton upFolderButton = new TooltipAwareButton(getChangeToParentDirectoryAction());
        setupButton(upFolderButton, upFolderAccessibleName, upFolderToolTipText);
        upFolderButton.setText(null);
        upFolderButton.setIcon(upFolderIcon);
        return upFolderButton;
    }

    protected JToggleButton createDetailsViewButton() {
        JToggleButton button = new TooltipAwareToggleButton(detailsViewIcon);
        setupButton(button, detailsViewButtonAccessibleName, detailsViewButtonToolTipText);
        Icon selectedDetailsViewIcon = UIManager.getIcon("FileChooser.detailsViewSelectedIcon");
        button.setSelectedIcon(selectedDetailsViewIcon);
        button.putClientProperty(DarkButtonUI.KEY_CORNER, AlignmentExt.RIGHT);
        button.addActionListener(filePane.getViewTypeAction(FilePane.VIEWTYPE_DETAILS));
        return button;
    }

    protected JToggleButton createListViewButton() {
        JToggleButton button = new TooltipAwareToggleButton(listViewIcon);
        setupButton(button, listViewButtonAccessibleName, listViewButtonToolTipText);
        Icon selectedListViewIcon = UIManager.getIcon("FileChooser.listViewSelectedIcon");
        button.setSelectedIcon(selectedListViewIcon);
        button.putClientProperty(DarkButtonUI.KEY_CORNER, AlignmentExt.LEFT);
        button.addActionListener(filePane.getViewTypeAction(FilePane.VIEWTYPE_LIST));
        button.setSelected(true);
        return button;
    }

    protected void setupButton(final AbstractButton button, final String accessibleName, final String tipText) {
        button.putClientProperty(DarkButtonUI.KEY_NO_BORDERLESS_OVERWRITE, true);
        button.putClientProperty(DarkButtonUI.KEY_SQUARE, true);
        button.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        button.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        button.setToolTipText(tipText);
        button.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, accessibleName);
    }

    @Override
    protected ListCellRenderer<Object> createFilterComboBoxRenderer() {
        return new DarkFilterComboBoxRenderer();
    }

    protected static class DarkButtonAreaLayout extends ButtonAreaLayout {

        protected DarkButtonAreaLayout() {
            topMargin = 5;
        }
    }

    public void clearIconCache() {
        getFileView().clearIconCache();
    }

    public BasicFileView getFileView() {
        if (fileView == null) fileView = createFileView();
        return fileView;
    }

    @Override
    public FileView getFileView(final JFileChooser fc) {
        return getFileView();
    }

    protected BasicFileView createFileView() {
        return new DarkFileView();
    }

    protected class DarkFileView extends BasicFileView {

        public Icon getIcon(final File f) {
            Icon icon = getCachedIcon(f);
            if (icon != null) {
                return icon;
            }
            icon = fileIcon;
            if (f != null) {
                FileSystemView fsv = getFileChooser().getFileSystemView();

                if (fsv.isFloppyDrive(f)) {
                    icon = floppyDriveIcon;
                } else if (fsv.isDrive(f)) {
                    icon = hardDriveIcon;
                } else if (fsv.isComputerNode(f)) {
                    icon = computerIcon;
                } else if (f.isDirectory()) {
                    icon = directoryIcon;
                } else {
                    try {
                        String mimeType = Files.probeContentType(f.toPath());
                        if (mimeType == null) mimeType = "";
                        if (mimeType.startsWith(MIME_IMAGE)) {
                            icon = imageFileIcon;
                        } else if (mimeType.startsWith(MIME_TEXT)) {
                            icon = textFileIcon;
                        }
                    } catch (final IOException e) {
                        e.printStackTrace();
                    }
                }
            }
            cacheIcon(f, icon);
            return icon;
        }
    }
}
