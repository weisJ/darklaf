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
package com.github.weisj.darklaf.ui.filechooser;

import com.github.weisj.darklaf.components.tooltip.TooltipAwareButton;
import com.github.weisj.darklaf.components.tooltip.TooltipAwareToggleButton;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.filechooser.FileSystemView;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.File;

public class DarkFileChooserUI extends DarkFileChooserUIBridge {

    public DarkFileChooserUI(final JFileChooser b) {
        super(b);
    }


    public static ComponentUI createUI(final JComponent c) {
        return new DarkFileChooserUI((JFileChooser) c);
    }

    @Override
    public void installComponents(final JFileChooser fc) {
        FileSystemView fsv = fc.getFileSystemView();

        fc.setBorder(new EmptyBorder(10, 10, 10, 10));
        fc.setLayout(new BorderLayout(0, 10));

        filePane = new DarkFilePane(new MetalFileChooserUIAccessor());
        fc.addPropertyChangeListener(filePane);

        // ********************************* //
        // **** Construct the top panel **** //
        // ********************************* //

        // Directory manipulation buttons
        JPanel topPanel = new JPanel(new BorderLayout(10, 0));
        JPanel topButtonPanel = new JPanel();
        topButtonPanel.setLayout(new BoxLayout(topButtonPanel, BoxLayout.LINE_AXIS));
        topPanel.add(topButtonPanel, BorderLayout.AFTER_LINE_ENDS);

        // Add the top panel to the fileChooser
        fc.add(topPanel, BorderLayout.NORTH);

        // ComboBox Label
        lookInLabel = new JLabel(lookInLabelText);
        lookInLabel.setDisplayedMnemonic(lookInLabelMnemonic);
        topPanel.add(lookInLabel, BorderLayout.BEFORE_LINE_BEGINS);

        // CurrentDir ComboBox
        directoryComboBox = new JComboBox<Object>() {
            public Dimension getPreferredSize() {
                Dimension d = super.getPreferredSize();
                // Must be small enough to not affect total width.
                d.width = 150;
                return d;
            }
        };
        directoryComboBox.putClientProperty(AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
                                            lookInLabelText);
        lookInLabel.setLabelFor(directoryComboBox);
        directoryComboBoxModel = createDirectoryComboBoxModel(fc);
        directoryComboBox.setModel(directoryComboBoxModel);
        directoryComboBox.addActionListener(directoryComboBoxAction);
        directoryComboBox.setRenderer(createDirectoryComboBoxRenderer(fc));
        directoryComboBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        directoryComboBox.setAlignmentY(JComponent.TOP_ALIGNMENT);
        directoryComboBox.setMaximumRowCount(8);

        topPanel.add(directoryComboBox, BorderLayout.CENTER);

        // Up Button
        JButton upFolderButton = new TooltipAwareButton(getChangeToParentDirectoryAction());
        upFolderButton.setText(null);
        upFolderButton.setIcon(upFolderIcon);
        upFolderButton.setToolTipText(upFolderToolTipText);
        upFolderButton.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                                         upFolderAccessibleName);
        upFolderButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        upFolderButton.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        upFolderButton.setMargin(shrinkwrap);

        topButtonPanel.add(upFolderButton);
        topButtonPanel.add(Box.createRigidArea(hstrut5));

        // Home Button
        File homeDir = fsv.getHomeDirectory();
        String toolTipText = homeFolderToolTipText;


        JButton b = new TooltipAwareButton(homeFolderIcon);
        b.setToolTipText(toolTipText);
        b.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                            homeFolderAccessibleName);
        b.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        b.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        b.setMargin(shrinkwrap);

        b.addActionListener(getGoHomeAction());
        topButtonPanel.add(b);
        topButtonPanel.add(Box.createRigidArea(hstrut5));

        // New Directory Button
        if (!UIManager.getBoolean("FileChooser.readOnly")) {
            b = new TooltipAwareButton(filePane.getNewFolderAction());
            b.setText(null);
            b.setIcon(newFolderIcon);
            b.setToolTipText(newFolderToolTipText);
            b.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                                newFolderAccessibleName);
            b.setAlignmentX(JComponent.LEFT_ALIGNMENT);
            b.setAlignmentY(JComponent.CENTER_ALIGNMENT);
            b.setMargin(shrinkwrap);
        }
        topButtonPanel.add(b);
        topButtonPanel.add(Box.createRigidArea(hstrut5));

        // View button group
        ButtonGroup viewButtonGroup = new ButtonGroup();

        // List Button
        listViewButton = new TooltipAwareToggleButton(listViewIcon);
        listViewButton.putClientProperty("JButton.square", Boolean.TRUE);
        listViewButton.setToolTipText(listViewButtonToolTipText);
        listViewButton.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                                         listViewButtonAccessibleName);


        listViewButton.setSelected(true);
        listViewButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        listViewButton.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        listViewButton.setMargin(shrinkwrap);
        listViewButton.addActionListener(filePane.getViewTypeAction(DarkFilePane.VIEWTYPE_LIST));
        topButtonPanel.add(listViewButton);
        topButtonPanel.add(Box.createRigidArea(hstrut5));
        viewButtonGroup.add(listViewButton);

        // Details Button
        detailsViewButton = new TooltipAwareToggleButton(detailsViewIcon);
        detailsViewButton.putClientProperty("JButton.square", Boolean.TRUE);
        detailsViewButton.setToolTipText(detailsViewButtonToolTipText);
        detailsViewButton.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                                            detailsViewButtonAccessibleName);
        detailsViewButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
        detailsViewButton.setAlignmentY(JComponent.CENTER_ALIGNMENT);
        detailsViewButton.setMargin(shrinkwrap);
        detailsViewButton.addActionListener(filePane.getViewTypeAction(DarkFilePane.VIEWTYPE_DETAILS));
        topButtonPanel.add(detailsViewButton);
        viewButtonGroup.add(detailsViewButton);

        topButtonPanel.add(Box.createGlue());

        filePane.addPropertyChangeListener(e -> {
            if ("viewType".equals(e.getPropertyName())) {
                int viewType = filePane.getViewType();
                switch (viewType) {
                    case DarkFilePane.VIEWTYPE_LIST:
                        listViewButton.setSelected(true);
                        break;

                    case DarkFilePane.VIEWTYPE_DETAILS:
                        detailsViewButton.setSelected(true);
                        break;
                }
            }
        });

        // ************************************** //
        // ******* Add the directory pane ******* //
        // ************************************** //
        fc.add(getAccessoryPanel(), BorderLayout.AFTER_LINE_ENDS);
        JComponent accessory = fc.getAccessory();
        if (accessory != null) {
            getAccessoryPanel().add(accessory);
        }
        filePane.setPreferredSize(LIST_PREF_SIZE);
        fc.add(filePane, BorderLayout.CENTER);

        // ********************************** //
        // **** Construct the bottom panel ** //
        // ********************************** //
        JPanel bottomPanel = getBottomPanel();
        bottomPanel.setLayout(new BoxLayout(bottomPanel, BoxLayout.Y_AXIS));
        fc.add(bottomPanel, BorderLayout.SOUTH);

        // FileName label and textfield
        JPanel fileNamePanel = new JPanel();
        fileNamePanel.setLayout(new BoxLayout(fileNamePanel, BoxLayout.LINE_AXIS));
        bottomPanel.add(fileNamePanel);
        bottomPanel.add(Box.createRigidArea(vstrut5));

        fileNameLabel = new AlignedLabel();
        populateFileNameLabel();
        fileNamePanel.add(fileNameLabel);

        fileNameTextField = new FileTextField();
        fileNamePanel.add(fileNameTextField);
        fileNameLabel.setLabelFor(fileNameTextField);
        fileNameTextField.addFocusListener(
                new FocusAdapter() {
                    public void focusGained(final FocusEvent e) {
                        if (!getFileChooser().isMultiSelectionEnabled()) {
                            filePane.clearSelection();
                        }
                    }
                }
        );
        if (fc.isMultiSelectionEnabled()) {
            setFileName(fileNameString(fc.getSelectedFiles()));
        } else {
            setFileName(fileNameString(fc.getSelectedFile()));
        }


        // Filetype label and combobox
        JPanel filesOfTypePanel = new JPanel();
        filesOfTypePanel.setLayout(new BoxLayout(filesOfTypePanel, BoxLayout.LINE_AXIS));
        bottomPanel.add(filesOfTypePanel);

        AlignedLabel filesOfTypeLabel = new AlignedLabel(filesOfTypeLabelText);
        filesOfTypeLabel.setDisplayedMnemonic(filesOfTypeLabelMnemonic);
        filesOfTypePanel.add(filesOfTypeLabel);

        filterComboBoxModel = createFilterComboBoxModel();
        fc.addPropertyChangeListener(filterComboBoxModel);
        filterComboBox = new JComboBox<>(filterComboBoxModel);
        filterComboBoxModel.addListDataListener(new ListDataListener() {
            @Override
            public void intervalAdded(final ListDataEvent e) {
                filterComboBox.setEnabled(filterComboBox.getItemCount() > 1);
            }

            @Override
            public void intervalRemoved(final ListDataEvent e) {
                filterComboBox.setEnabled(filterComboBox.getItemCount() > 1);
            }

            @Override
            public void contentsChanged(final ListDataEvent e) {
                filterComboBox.setEnabled(filterComboBox.getItemCount() > 1);
            }
        });
        filterComboBox.putClientProperty(AccessibleContext.ACCESSIBLE_DESCRIPTION_PROPERTY,
                                         filesOfTypeLabelText);
        filesOfTypeLabel.setLabelFor(filterComboBox);
        filterComboBox.setRenderer(createFilterComboBoxRenderer());
        filesOfTypePanel.add(filterComboBox);

        // buttons
        getButtonPanel().setLayout(new DarkButtonAreaLayout());

        approveButton = new TooltipAwareButton(getApproveButtonText(fc));
        // Note: Metal does not use mnemonics for approve and cancel
        approveButton.addActionListener(getApproveSelectionAction());
        approveButton.setToolTipText(getApproveButtonToolTipText(fc));
        getButtonPanel().add(approveButton);

        cancelButton = new TooltipAwareButton(cancelButtonText);
        cancelButton.setToolTipText(cancelButtonToolTipText);
        cancelButton.addActionListener(getCancelSelectionAction());
        getButtonPanel().add(cancelButton);

        if (fc.getControlButtonsAreShown()) {
            addControlButtons();
        }

        groupLabels(new AlignedLabel[]{fileNameLabel, filesOfTypeLabel});
    }

    protected static class DarkButtonAreaLayout extends ButtonAreaLayout {

        protected DarkButtonAreaLayout() {
            topMargin = 10;
        }
    }
}
