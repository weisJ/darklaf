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
package com.github.weisj.darklaf.ui.filechooser;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.util.*;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;
import javax.swing.filechooser.FileSystemView;
import javax.swing.plaf.basic.BasicDirectoryModel;
import javax.swing.table.*;

import sun.awt.shell.ShellFolder;
import sun.awt.shell.ShellFolderColumnInfo;
import sun.swing.FilePane;

import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellRenderer;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * <b>WARNING:</b> This class is an implementation detail and is only public so that it can be used
 * by two packages. You should NOT consider this public API.
 *
 * <p>
 * This component is intended to be used in a subclass of javax.swing.plaf.basic.BasicFileChooserUI.
 * It realies heavily on the implementation of BasicFileChooserUI, and is intended to be API
 * compatible with earlier implementations of MetalFileChooserUI and WindowsFileChooserUI.
 *
 * @author Leif Samuelsson
 */
public abstract class DarkFilePaneUIBridge extends JPanel implements PropertyChangeListener {

    // Constants for actions. These are used for the actions' ACTION_COMMAND_KEY
    // and as keys in the action maps for FilePane and the corresponding UI classes

    public static final String ACTION_APPROVE_SELECTION = "approveSelection";
    public static final String ACTION_CANCEL = "cancelSelection";
    public static final String ACTION_EDIT_FILE_NAME = "editFileName";
    public static final String ACTION_REFRESH = "refresh";
    public static final String ACTION_CHANGE_TO_PARENT_DIRECTORY = "Go Up";
    public static final String ACTION_NEW_FOLDER = "New Folder";
    public static final String ACTION_VIEW_LIST = "viewTypeList";
    public static final String ACTION_VIEW_DETAILS = "viewTypeDetails";
    // "enums" for setViewType()
    public static final int VIEWTYPE_LIST = 0;
    public static final int VIEWTYPE_DETAILS = 1;
    protected static final int VIEWTYPE_COUNT = 2;
    protected static final Cursor waitCursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
    protected static final int COLUMN_FILENAME = 0;
    protected static final int COLUMN_SIZE = 1;
    protected static final FocusListener repaintListener = new FocusListener() {
        public void focusGained(final FocusEvent fe) {
            repaintSelection(fe.getSource());
        }

        public void focusLost(final FocusEvent fe) {
            repaintSelection(fe.getSource());
        }

        protected void repaintSelection(final Object source) {
            if (source instanceof JList) {
                repaintListSelection((JList<?>) source);
            } else if (source instanceof JTable) {
                repaintTableSelection((JTable) source);
            }
        }

        protected void repaintListSelection(final JList<?> list) {
            int[] indices = list.getSelectedIndices();
            for (int i : indices) {
                Rectangle bounds = list.getCellBounds(i, i);
                if (bounds != null) {
                    list.repaint(bounds);
                }
            }
        }

        protected void repaintTableSelection(final JTable table) {
            int minRow = table.getSelectionModel().getMinSelectionIndex();
            int maxRow = table.getSelectionModel().getMaxSelectionIndex();
            if (minRow == -1 || maxRow == -1) {
                return;
            }

            int col0 = table.convertColumnIndexToView(COLUMN_FILENAME);

            Rectangle first = table.getCellRect(minRow, col0, false);
            Rectangle last = table.getCellRect(maxRow, col0, false);
            Rectangle dirty = first.union(last);
            table.repaint(dirty);
        }
    };
    protected Action[] actions;
    protected int viewType = -1;
    protected final JPanel[] viewPanels = new JPanel[VIEWTYPE_COUNT];
    protected JPanel currentViewPanel;
    protected String[] viewTypeActionNames;
    protected String filesListAccessibleName = null;
    protected String filesDetailsAccessibleName = null;
    protected JPopupMenu contextMenu;
    protected JMenu viewMenu;
    protected String viewMenuLabelText;
    protected String refreshActionLabelText;
    protected String newFolderActionLabelText;
    protected String kiloByteString;
    protected String megaByteString;
    protected String gigaByteString;
    protected String renameErrorTitleText;
    protected String renameErrorText;
    protected String renameErrorFileExistsText;
    protected final boolean smallIconsView = false;
    protected Border listViewBorder;
    protected Color listViewBackground;
    protected boolean listViewWindowsStyle;
    protected boolean readOnly;
    protected boolean fullRowSelection = true;

    protected ListSelectionModel listSelectionModel;
    protected JList<?> list;
    protected JTable detailsTable;
    // Provides a way to recognize a newly created folder, so it can
    // be selected when it appears in the model.
    protected File newFolderFile;
    // Used for accessing methods in the corresponding UI class
    protected final FileChooserUIAccessor fileChooserUIAccessor;
    protected DetailsTableModel detailsTableModel;
    protected DetailsTableRowSorter rowSorter;
    protected final KeyListener detailsKeyListener = new KeyAdapter() {
        protected final long timeFactor;

        protected final StringBuilder typedString = new StringBuilder();

        protected long lastTime = 1000L;

        {
            Long l = (Long) UIManager.get("Table.timeFactor");
            timeFactor = (l != null) ? l : 1000L;
        }

        /**
         * Moves the keyboard focus to the first element whose prefix matches the sequence of alphanumeric
         * keys pressed by the user with delay less than value of <code>timeFactor
         * </code>. Subsequent same key presses move the keyboard focus to the next object that starts with
         * the same letter until another key is pressed, then it is treated as the prefix with appropriate
         * number of the same letters followed by first typed another letter.
         */
        public void keyTyped(final KeyEvent e) {
            BasicDirectoryModel model = getModel();
            int rowCount = model.getSize();

            if (detailsTable == null || rowCount == 0 || e.isAltDown() || e.isControlDown() || e.isMetaDown()) {
                return;
            }

            InputMap inputMap = detailsTable.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
            KeyStroke key = KeyStroke.getKeyStrokeForEvent(e);

            if (inputMap != null && inputMap.get(key) != null) {
                return;
            }

            int startIndex = detailsTable.getSelectionModel().getLeadSelectionIndex();

            if (startIndex < 0) {
                startIndex = 0;
            }

            if (startIndex >= rowCount) {
                startIndex = rowCount - 1;
            }

            char c = e.getKeyChar();

            long time = e.getWhen();

            if (time - lastTime < timeFactor) {
                if (typedString.length() == 1 && typedString.charAt(0) == c) {
                    // Subsequent same key presses move the keyboard focus to the next
                    // object that starts with the same letter.
                    startIndex++;
                } else {
                    typedString.append(c);
                }
            } else {
                startIndex++;

                typedString.setLength(0);
                typedString.append(c);
            }

            lastTime = time;

            if (startIndex >= rowCount) {
                startIndex = 0;
            }

            // Find next file
            int index = getNextMatch(startIndex, rowCount - 1);

            if (index < 0 && startIndex > 0) { // wrap
                index = getNextMatch(0, startIndex - 1);
            }

            if (index >= 0) {
                detailsTable.getSelectionModel().setSelectionInterval(index, index);

                Rectangle cellRect =
                        detailsTable.getCellRect(index, detailsTable.convertColumnIndexToView(COLUMN_FILENAME), false);
                detailsTable.scrollRectToVisible(cellRect);
            }
        }

        protected int getNextMatch(final int startIndex, final int finishIndex) {
            BasicDirectoryModel model = getModel();
            JFileChooser fileChooser = getFileChooser();
            DetailsTableRowSorter rowSorter = getRowSorter();

            String prefix = typedString.toString().toLowerCase();

            // Search element
            for (int index = startIndex; index <= finishIndex; index++) {
                File file = (File) model.getElementAt(rowSorter.convertRowIndexToModel(index));

                String fileName = fileChooser.getName(file).toLowerCase();

                if (fileName.startsWith(prefix)) {
                    return index;
                }
            }

            return -1;
        }
    };
    protected Action newFolderAction;
    protected Handler handler;
    int lastIndex = -1;
    File editFile = null;
    JTextField editCell = null;
    protected final FocusListener editorFocusListener = new FocusAdapter() {
        public void focusLost(final FocusEvent e) {
            // Todo
            if (!e.isTemporary()) {
                // applyEdit();
            }
        }
    };

    public DarkFilePaneUIBridge(final FileChooserUIAccessor fileChooserUIAccessor) {
        super(new BorderLayout());

        this.fileChooserUIAccessor = fileChooserUIAccessor;

        installDefaults();
        createActionMap();
    }

    protected static void recursivelySetInheritsPopupMenu(final Container container, final boolean b) {
        if (container instanceof JComponent) {
            ((JComponent) container).setInheritsPopupMenu(b);
        }
        int n = container.getComponentCount();
        for (int i = 0; i < n; i++) {
            recursivelySetInheritsPopupMenu((Container) container.getComponent(i), b);
        }
    }

    public static void addActionsToMap(final ActionMap map, final Action[] actions) {
        if (map != null && actions != null) {
            for (Action a : actions) {
                String cmd = (String) a.getValue(Action.ACTION_COMMAND_KEY);
                if (cmd == null) {
                    cmd = (String) a.getValue(Action.NAME);
                }
                map.put(cmd, a);
            }
        }
    }

    public static boolean canWrite(final File f, final JFileChooser chooser) {
        // Return false for non FileSystem files or if file doesn't exist.
        if (!f.exists()) {
            return false;
        }

        try {
            if (f instanceof ShellFolder) {
                return f.canWrite();
            } else {
                if (usesShellFolder(chooser)) {
                    try {
                        return ShellFolder.getShellFolder(f).canWrite();
                    } catch (FileNotFoundException ex) {
                        // File doesn't exist
                        return false;
                    }
                } else {
                    // Ordinary file
                    return f.canWrite();
                }
            }
        } catch (SecurityException e) {
            return false;
        }
    }

    /**
     * Returns true if specified FileChooser should use ShellFolder
     *
     * @param chooser the chooser
     * @return the boolean
     */
    public static boolean usesShellFolder(final JFileChooser chooser) {
        Boolean prop = (Boolean) chooser.getClientProperty("FileChooser.useShellFolder");

        return prop == null ? chooser.getFileSystemView().equals(FileSystemView.getFileSystemView()) : prop;
    }

    public void uninstallUI() {
        if (getModel() != null) {
            getModel().removePropertyChangeListener(this);
        }
    }

    protected BasicDirectoryModel getModel() {
        return fileChooserUIAccessor.getModel();
    }

    public int getViewType() {
        return viewType;
    }

    public void setViewType(final int viewType) {
        if (viewType == this.viewType) {
            return;
        }

        int oldValue = this.viewType;
        this.viewType = viewType;

        JPanel createdViewPanel = null;
        Component newFocusOwner = null;

        switch (viewType) {
            case VIEWTYPE_LIST:
                if (viewPanels[viewType] == null) {
                    createdViewPanel = fileChooserUIAccessor.createList();
                    if (createdViewPanel == null) {
                        createdViewPanel = createList();
                    }

                    list = findChildComponent(createdViewPanel, JList.class);
                    if (listSelectionModel == null) {
                        listSelectionModel = list.getSelectionModel();
                        if (detailsTable != null) {
                            detailsTable.setSelectionModel(listSelectionModel);
                        }
                    } else {
                        list.setSelectionModel(listSelectionModel);
                    }
                }
                list.setLayoutOrientation(JList.VERTICAL_WRAP);
                newFocusOwner = list;
                break;

            case VIEWTYPE_DETAILS:
                if (viewPanels[viewType] == null) {
                    createdViewPanel = fileChooserUIAccessor.createDetailsView();
                    if (createdViewPanel == null) {
                        createdViewPanel = createDetailsView();
                    }

                    detailsTable = findChildComponent(createdViewPanel, JTable.class);
                    if (listSelectionModel != null) {
                        detailsTable.setSelectionModel(listSelectionModel);
                    }
                }
                newFocusOwner = detailsTable;
                break;
        }

        if (createdViewPanel != null) {
            viewPanels[viewType] = createdViewPanel;
            recursivelySetInheritsPopupMenu(createdViewPanel, true);
        }

        boolean isFocusOwner = false;

        if (currentViewPanel != null) {
            Component owner = DefaultKeyboardFocusManager.getCurrentKeyboardFocusManager().getPermanentFocusOwner();

            isFocusOwner = owner == detailsTable || owner == list;

            remove(currentViewPanel);
        }

        currentViewPanel = viewPanels[viewType];
        add(currentViewPanel, BorderLayout.CENTER);

        if (isFocusOwner && newFocusOwner != null) {
            newFocusOwner.requestFocusInWindow();
        }

        revalidate();
        repaint();
        updateViewMenu();
        firePropertyChange(DarkFileChooserUI.KEY_VIEW_TYPE, oldValue, viewType);
    }

    public Action getViewTypeAction(final int viewType) {
        return new ViewTypeAction(viewType);
    }

    protected void installDefaults() {
        Locale l = getFileChooser().getLocale();

        listViewBorder = UIManager.getBorder("FileChooser.listViewBorder");
        listViewBackground = UIManager.getColor("FileChooser.listViewBackground");
        listViewWindowsStyle = UIManager.getBoolean("FileChooser.listViewWindowsStyle");
        readOnly = UIManager.getBoolean("FileChooser.readOnly");

        // TUDU: On windows, get the following localized strings from the OS
        viewMenuLabelText = UIManager.getString("FileChooser.viewMenuLabelText", l);
        refreshActionLabelText = UIManager.getString("FileChooser.refreshActionLabelText", l);
        newFolderActionLabelText = UIManager.getString("FileChooser.newFolderActionLabelText", l);

        viewTypeActionNames = new String[VIEWTYPE_COUNT];
        viewTypeActionNames[VIEWTYPE_LIST] = UIManager.getString("FileChooser.listViewActionLabelText", l);
        viewTypeActionNames[VIEWTYPE_DETAILS] = UIManager.getString("FileChooser.detailsViewActionLabelText", l);

        kiloByteString = UIManager.getString("FileChooser.fileSizeKiloBytes", l);
        megaByteString = UIManager.getString("FileChooser.fileSizeMegaBytes", l);
        gigaByteString = UIManager.getString("FileChooser.fileSizeGigaBytes", l);
        fullRowSelection = UIManager.getBoolean("FileView.fullRowSelection");

        filesListAccessibleName = UIManager.getString("FileChooser.filesListAccessibleName", l);
        filesDetailsAccessibleName = UIManager.getString("FileChooser.filesDetailsAccessibleName", l);

        renameErrorTitleText = UIManager.getString("FileChooser.renameErrorTitleText", l);
        renameErrorText = UIManager.getString("FileChooser.renameErrorText", l);
        renameErrorFileExistsText = UIManager.getString("FileChooser.renameErrorFileExistsText", l);
    }

    /**
     * Fetches the command list for the FilePane. These commands are useful for binding to events, such
     * as in a keymap.
     *
     * @return the command list
     */
    public Action[] getActions() {
        if (actions == null) {
            @SuppressWarnings("serial")
            // JDK-implementation class
            class FilePaneAction extends AbstractAction {
                FilePaneAction(final String name) {
                    this(name, name);
                }

                FilePaneAction(final String name, final String cmd) {
                    super(name);
                    putValue(Action.ACTION_COMMAND_KEY, cmd);
                }

                public void actionPerformed(final ActionEvent e) {
                    String cmd = (String) getValue(Action.ACTION_COMMAND_KEY);

                    if (Objects.equals(cmd, ACTION_CANCEL)) {
                        if (editFile != null) {
                            cancelEdit();
                        } else {
                            getFileChooser().cancelSelection();
                        }
                    } else if (Objects.equals(cmd, ACTION_EDIT_FILE_NAME)) {
                        JFileChooser fc = getFileChooser();
                        int index = listSelectionModel.getMinSelectionIndex();
                        if (index >= 0 && editFile == null
                                && (!fc.isMultiSelectionEnabled() || fc.getSelectedFiles().length <= 1)) {

                            editFileName(index);
                        }
                    } else if (Objects.equals(cmd, ACTION_REFRESH)) {
                        getFileChooser().rescanCurrentDirectory();
                    }
                }

                public boolean isEnabled() {
                    String cmd = (String) getValue(Action.ACTION_COMMAND_KEY);
                    if (Objects.equals(cmd, ACTION_CANCEL)) {
                        return getFileChooser().isEnabled();
                    } else if (Objects.equals(cmd, ACTION_EDIT_FILE_NAME)) {
                        return !readOnly && getFileChooser().isEnabled();
                    } else {
                        return true;
                    }
                }
            }

            ArrayList<Action> actionList = new ArrayList<>(8);
            Action action;

            actionList.add(new FilePaneAction(ACTION_CANCEL));
            actionList.add(new FilePaneAction(ACTION_EDIT_FILE_NAME));
            actionList.add(new FilePaneAction(refreshActionLabelText, ACTION_REFRESH));

            action = fileChooserUIAccessor.getApproveSelectionAction();
            if (action != null) {
                actionList.add(action);
            }
            action = fileChooserUIAccessor.getChangeToParentDirectoryAction();
            if (action != null) {
                actionList.add(action);
            }
            action = getNewFolderAction();
            if (action != null) {
                actionList.add(action);
            }
            action = getViewTypeAction(VIEWTYPE_LIST);
            if (action != null) {
                actionList.add(action);
            }
            action = getViewTypeAction(VIEWTYPE_DETAILS);
            if (action != null) {
                actionList.add(action);
            }
            actions = actionList.toArray(new Action[0]);
        }

        return Arrays.copyOf(actions, actions.length);
    }

    protected void createActionMap() {
        addActionsToMap(super.getActionMap(), getActions());
    }

    protected void updateListRowCount(final JList<?> list) {
        if (smallIconsView) {
            list.setVisibleRowCount(getModel().getSize() / 3);
        } else {
            list.setVisibleRowCount(-1);
        }
    }

    public abstract JPanel createList();

    protected DetailsTableModel getDetailsTableModel() {
        if (detailsTableModel == null) {
            detailsTableModel = new DetailsTableModel(getFileChooser());
        }
        return detailsTableModel;
    }

    protected void updateDetailsColumnModel(final JTable table) {
        if (table != null) {
            ShellFolderColumnInfo[] columns = detailsTableModel.getColumns();

            TableColumnModel columnModel = new DefaultTableColumnModel();
            for (int i = 0; i < columns.length; i++) {
                ShellFolderColumnInfo dataItem = columns[i];
                TableColumn column = new TableColumn(i);

                String title = dataItem.getTitle();
                if (title != null && title.startsWith("FileChooser.") && title.endsWith("HeaderText")) {
                    // the column must have a string resource that we try to get
                    String uiTitle = UIManager.getString(title, table.getLocale());
                    if (uiTitle != null) {
                        title = uiTitle;
                    }
                }
                column.setHeaderValue(title);

                Integer width = dataItem.getWidth();
                if (width != null) {
                    column.setPreferredWidth(width);
                    // otherwise we let JTable to decide the actual width
                }

                columnModel.addColumn(column);
            }

            // Install cell editor for editing file name
            if (!readOnly && columnModel.getColumnCount() > COLUMN_FILENAME) {
                columnModel.getColumn(COLUMN_FILENAME).setCellEditor(getDetailsTableCellEditor());
            }

            table.setColumnModel(columnModel);
            table.getColumnModel().setColumnMargin(0);
        }
    }

    protected abstract TableCellEditor getDetailsTableCellEditor();

    public abstract JPanel createDetailsView();

    protected void fixNameColumnWidth(final int viewWidth) {
        TableColumn nameCol = detailsTable.getColumnModel().getColumn(COLUMN_FILENAME);
        int tableWidth = detailsTable.getPreferredSize().width;

        if (tableWidth < viewWidth) {
            nameCol.setPreferredWidth(nameCol.getPreferredWidth() + viewWidth - tableWidth);
        }
    }

    /**
     * Creates a selection listener for the list of files and directories.
     *
     * @return a <code>ListSelectionListener</code>
     */
    public ListSelectionListener createListSelectionListener() {
        return fileChooserUIAccessor.createListSelectionListener();
    }

    protected int getEditIndex() {
        return lastIndex;
    }

    protected void setEditIndex(final int i) {
        lastIndex = i;
    }

    protected void resetEditIndex() {
        lastIndex = -1;
    }

    protected abstract void cancelEdit();

    /** @param index visual index of the file to be edited */
    protected abstract void editFileName(final int index);

    protected void applyEdit() {
        if (editFile != null && editFile.exists()) {
            JFileChooser chooser = getFileChooser();
            String oldDisplayName = chooser.getName(editFile);
            String oldFileName = editFile.getName();
            String newDisplayName = editCell.getText().trim();
            String newFileName;

            if (!newDisplayName.equals(oldDisplayName)) {
                newFileName = newDisplayName;
                // Check if extension is hidden from user
                int i1 = oldFileName.length();
                int i2 = oldDisplayName.length();
                if (i1 > i2 && oldFileName.charAt(i2) == '.') {
                    newFileName = newDisplayName + oldFileName.substring(i2);
                }

                // rename
                FileSystemView fsv = chooser.getFileSystemView();
                File f2 = fsv.createFileObject(editFile.getParentFile(), newFileName);
                if (f2.exists()) {
                    JOptionPane.showMessageDialog(chooser, MessageFormat.format(renameErrorFileExistsText, oldFileName),
                            renameErrorTitleText, JOptionPane.ERROR_MESSAGE);
                } else {
                    if (getModel().renameFile(editFile, f2)) {
                        if (fsv.isParent(chooser.getCurrentDirectory(), f2)) {
                            if (chooser.isMultiSelectionEnabled()) {
                                chooser.setSelectedFiles(new File[] {f2});
                            } else {
                                chooser.setSelectedFile(f2);
                            }
                        } else {
                            // Could be because of delay in updating Desktop folder
                            // chooser.setSelectedFile(null);
                        }
                    } else {
                        JOptionPane.showMessageDialog(chooser, MessageFormat.format(renameErrorText, oldFileName),
                                renameErrorTitleText, JOptionPane.ERROR_MESSAGE);
                    }
                }
            }
        }
        if (detailsTable != null && detailsTable.isEditing()) {
            detailsTable.getCellEditor().stopCellEditing();
        }
        cancelEdit();
    }

    @SuppressWarnings("serial") // anonymous class inside
    public Action getNewFolderAction() {
        if (!readOnly && newFolderAction == null) {
            newFolderAction = new AbstractAction(newFolderActionLabelText) {
                protected Action basicNewFolderAction;

                // Initializer
                {
                    putValue(Action.ACTION_COMMAND_KEY, FilePane.ACTION_NEW_FOLDER);

                    File currentDirectory = getFileChooser().getCurrentDirectory();
                    if (currentDirectory != null) {
                        setEnabled(canWrite(currentDirectory, getFileChooser()));
                    }
                }

                public void actionPerformed(final ActionEvent ev) {
                    if (basicNewFolderAction == null) {
                        basicNewFolderAction = fileChooserUIAccessor.getNewFolderAction();
                    }
                    JFileChooser fc = getFileChooser();
                    File oldFile = fc.getSelectedFile();
                    basicNewFolderAction.actionPerformed(ev);
                    File newFile = fc.getSelectedFile();
                    if (newFile != null && !newFile.equals(oldFile) && newFile.isDirectory()) {
                        newFolderFile = newFile;
                    }
                }
            };
        }
        return newFolderAction;
    }

    @SuppressWarnings("deprecation")
    void setFileSelected() {
        if (getFileChooser().isMultiSelectionEnabled() && !isDirectorySelected()) {
            File[] files = getFileChooser().getSelectedFiles(); // Should be selected
            Object[] selectedObjects = list.getSelectedValues(); // Are actually selected

            listSelectionModel.setValueIsAdjusting(true);
            try {
                int lead = listSelectionModel.getLeadSelectionIndex();
                int anchor = listSelectionModel.getAnchorSelectionIndex();

                Arrays.sort(files);
                Arrays.sort(selectedObjects);

                int shouldIndex = 0;
                int actuallyIndex = 0;

                // Remove files that shouldn't be selected and add files which should be selected
                // Note: Assume files are already sorted in compareTo order.
                while (shouldIndex < files.length && actuallyIndex < selectedObjects.length) {
                    int comparison = files[shouldIndex].compareTo((File) selectedObjects[actuallyIndex]);
                    if (comparison < 0) {
                        doSelectFile(files[shouldIndex++]);
                    } else if (comparison > 0) {
                        doDeselectFile(selectedObjects[actuallyIndex++]);
                    } else {
                        // Do nothing
                        shouldIndex++;
                        actuallyIndex++;
                    }
                }

                while (shouldIndex < files.length) {
                    doSelectFile(files[shouldIndex++]);
                }

                while (actuallyIndex < selectedObjects.length) {
                    doDeselectFile(selectedObjects[actuallyIndex++]);
                }

                // restore the anchor and lead
                if (listSelectionModel instanceof DefaultListSelectionModel) {
                    ((DefaultListSelectionModel) listSelectionModel).moveLeadSelectionIndex(lead);
                    listSelectionModel.setAnchorSelectionIndex(anchor);
                }
            } finally {
                listSelectionModel.setValueIsAdjusting(false);
            }
        } else {
            JFileChooser chooser = getFileChooser();
            File f;
            if (isDirectorySelected()) {
                f = getDirectory();
            } else {
                f = chooser.getSelectedFile();
            }
            int i;
            if (f != null && (i = getModel().indexOf(f)) >= 0) {
                int viewIndex = getRowSorter().convertRowIndexToView(i);
                listSelectionModel.setSelectionInterval(viewIndex, viewIndex);
                ensureIndexIsVisible(viewIndex);
            } else {
                clearSelection();
            }
        }
    }

    protected void doSelectFile(final File fileToSelect) {
        int index = getModel().indexOf(fileToSelect);
        // could be missed in the current directory if it changed
        if (index >= 0) {
            index = getRowSorter().convertRowIndexToView(index);
            listSelectionModel.addSelectionInterval(index, index);
        }
    }

    protected void doDeselectFile(final Object fileToDeselect) {
        int index = getRowSorter().convertRowIndexToView(getModel().indexOf(fileToDeselect));
        listSelectionModel.removeSelectionInterval(index, index);
    }

    protected void doSelectedFileChanged(final PropertyChangeEvent e) {
        applyEdit();
        File f = (File) e.getNewValue();
        JFileChooser fc = getFileChooser();
        if (f != null && ((fc.isFileSelectionEnabled() && !f.isDirectory())
                || (f.isDirectory() && fc.isDirectorySelectionEnabled()))) {

            setFileSelected();
        }
    }

    protected void doSelectedFilesChanged(final PropertyChangeEvent e) {
        applyEdit();
        File[] files = (File[]) e.getNewValue();
        JFileChooser fc = getFileChooser();
        if (files != null && files.length > 0
                && (files.length > 1 || fc.isDirectorySelectionEnabled() || !files[0].isDirectory())) {
            setFileSelected();
        }
    }

    protected void doDirectoryChanged(final PropertyChangeEvent e) {
        getDetailsTableModel().updateColumnInfo();

        JFileChooser fc = getFileChooser();
        FileSystemView fsv = fc.getFileSystemView();

        applyEdit();
        resetEditIndex();
        ensureIndexIsVisible(0);
        File currentDirectory = fc.getCurrentDirectory();
        if (currentDirectory != null) {
            if (!readOnly) {
                getNewFolderAction().setEnabled(canWrite(currentDirectory, getFileChooser()));
            }
            fileChooserUIAccessor.getChangeToParentDirectoryAction().setEnabled(!fsv.isRoot(currentDirectory));
        }
        if (list != null) {
            list.clearSelection();
        }
    }

    protected void doFilterChanged(final PropertyChangeEvent e) {
        applyEdit();
        resetEditIndex();
        clearSelection();
    }

    protected void doFileSelectionModeChanged(final PropertyChangeEvent e) {
        applyEdit();
        resetEditIndex();
        clearSelection();
    }

    protected void doMultiSelectionChanged(final PropertyChangeEvent e) {
        if (getFileChooser().isMultiSelectionEnabled()) {
            listSelectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        } else {
            listSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            clearSelection();
            getFileChooser().setSelectedFiles(null);
        }
    }

    /*
     * Listen for filechooser property changes, such as the selected file changing, or the type of the
     * dialog changing.
     */
    public void propertyChange(final PropertyChangeEvent e) {
        if (viewType == -1) {
            setViewType(VIEWTYPE_LIST);
        }

        String s = e.getPropertyName();
        switch (s) {
            case JFileChooser.SELECTED_FILE_CHANGED_PROPERTY:
                doSelectedFileChanged(e);
                break;
            case JFileChooser.SELECTED_FILES_CHANGED_PROPERTY:
                doSelectedFilesChanged(e);
                break;
            case JFileChooser.DIRECTORY_CHANGED_PROPERTY:
                doDirectoryChanged(e);
                break;
            case JFileChooser.FILE_FILTER_CHANGED_PROPERTY:
                doFilterChanged(e);
                break;
            case JFileChooser.FILE_SELECTION_MODE_CHANGED_PROPERTY:
                doFileSelectionModeChanged(e);
                break;
            case JFileChooser.MULTI_SELECTION_ENABLED_CHANGED_PROPERTY:
                doMultiSelectionChanged(e);
                break;
            case JFileChooser.CANCEL_SELECTION:
                applyEdit();
                break;
            case "busy":
                setCursor((Boolean) e.getNewValue() ? waitCursor : null);
                break;
            case PropertyKey.COMPONENT_ORIENTATION:
                ComponentOrientation o = (ComponentOrientation) e.getNewValue();
                JFileChooser cc = (JFileChooser) e.getSource();
                if (o != e.getOldValue()) {
                    cc.applyComponentOrientation(o);
                }
                if (detailsTable != null) {
                    detailsTable.setComponentOrientation(o);
                    detailsTable.getParent().getParent().setComponentOrientation(o);
                }
                break;
        }
    }

    public void ensureFileIsVisible(final JFileChooser fc, final File f) {
        int modelIndex = getModel().indexOf(f);
        if (modelIndex >= 0) {
            ensureIndexIsVisible(getRowSorter().convertRowIndexToView(modelIndex));
        }
    }

    protected void ensureIndexIsVisible(final int i) {
        if (i >= 0) {
            if (list != null) {
                list.ensureIndexIsVisible(i);
            }
            if (detailsTable != null) {
                detailsTable.scrollRectToVisible(detailsTable.getCellRect(i, COLUMN_FILENAME, true));
            }
        }
    }

    protected DetailsTableRowSorter getRowSorter() {
        if (rowSorter == null) {
            rowSorter = new DetailsTableRowSorter();
        }
        return rowSorter;
    }

    /* The following methods are used by the PropertyChange Listener */
    public void rescanCurrentDirectory() {
        getModel().validateFileCache();
    }

    public void clearSelection() {
        if (listSelectionModel != null) {
            listSelectionModel.clearSelection();
            if (listSelectionModel instanceof DefaultListSelectionModel) {
                ((DefaultListSelectionModel) listSelectionModel).moveLeadSelectionIndex(-1);
                listSelectionModel.setAnchorSelectionIndex(-1);
            }
        }
    }

    public abstract JPopupMenu getComponentPopupMenu();

    protected JFileChooser getFileChooser() {
        return fileChooserUIAccessor.getFileChooser();
    }

    public abstract JMenu getViewMenu();

    protected abstract void updateViewMenu();

    protected abstract Handler getMouseHandler();

    /**
     * Property to remember whether a directory is currently selected in the UI.
     *
     * @return <code>true</code> iff a directory is currently selected.
     */
    protected boolean isDirectorySelected() {
        return fileChooserUIAccessor.isDirectorySelected();
    }

    /**
     * Property to remember the directory that is currently selected in the UI.
     *
     * @return the value of the <code>directory</code> property
     */
    protected File getDirectory() {
        return fileChooserUIAccessor.getDirectory();
    }

    protected <T> T findChildComponent(final Container container, final Class<T> cls) {
        int n = container.getComponentCount();
        for (int i = 0; i < n; i++) {
            Component comp = container.getComponent(i);
            if (cls.isInstance(comp)) {
                return cls.cast(comp);
            } else if (comp instanceof Container) {
                T c = findChildComponent((Container) comp, cls);
                if (c != null) {
                    return c;
                }
            }
        }
        return null;
    }

    // This interface is used to access methods in the FileChooserUI
    // that are not public.
    public interface FileChooserUIAccessor {
        JFileChooser getFileChooser();

        BasicDirectoryModel getModel();

        JPanel createList();

        JPanel createDetailsView();

        boolean isDirectorySelected();

        File getDirectory();

        Action getApproveSelectionAction();

        Action getChangeToParentDirectoryAction();

        Action getNewFolderAction();

        MouseListener createDoubleClickListener(JList<?> list);

        ListSelectionListener createListSelectionListener();
    }

    @SuppressWarnings("serial")
    // JDK-implementation class
    protected class ViewTypeAction extends AbstractAction {
        protected final int viewType;

        ViewTypeAction(final int viewType) {
            super(viewTypeActionNames[viewType]);
            this.viewType = viewType;

            String cmd;
            switch (viewType) {
                case VIEWTYPE_LIST:
                    cmd = ACTION_VIEW_LIST;
                    break;
                case VIEWTYPE_DETAILS:
                    cmd = ACTION_VIEW_DETAILS;
                    break;
                default:
                    cmd = (String) getValue(Action.NAME);
            }
            putValue(Action.ACTION_COMMAND_KEY, cmd);
        }

        public void actionPerformed(final ActionEvent e) {
            setViewType(viewType);
        }
    }

    /** This model allows for sorting JList */
    @SuppressWarnings("serial") // JDK-implementation class
    protected class SortableListModel extends AbstractListModel<Object>
            implements TableModelListener, RowSorterListener {

        public SortableListModel() {
            getDetailsTableModel().addTableModelListener(this);
            getRowSorter().addRowSorterListener(this);
        }

        public void tableChanged(final TableModelEvent e) {
            fireContentsChanged(this, 0, getSize());
        }

        public int getSize() {
            return getModel().getSize();
        }

        public Object getElementAt(final int index) {
            // JList doesn't support RowSorter so far, so we put it into the list model
            return getModel().getElementAt(getRowSorter().convertRowIndexToModel(index));
        }

        public void sorterChanged(final RowSorterEvent e) {
            fireContentsChanged(this, 0, getSize());
        }
    }

    @SuppressWarnings("serial")
    // JDK-implementation class
    class DetailsTableModel extends AbstractTableModel implements ListDataListener {
        final JFileChooser chooser;
        final BasicDirectoryModel directoryModel;

        ShellFolderColumnInfo[] columns;
        int[] columnMap;

        DetailsTableModel(final JFileChooser fc) {
            this.chooser = fc;
            directoryModel = getModel();
            directoryModel.addListDataListener(this);

            updateColumnInfo();
        }

        void updateColumnInfo() {
            File dir = chooser.getCurrentDirectory();
            if (dir != null && usesShellFolder(chooser)) {
                try {
                    dir = ShellFolder.getShellFolder(dir);
                } catch (FileNotFoundException e) {
                    // Leave dir without changing
                }
            }

            ShellFolderColumnInfo[] allColumns = ShellFolder.getFolderColumns(dir);

            ArrayList<ShellFolderColumnInfo> visibleColumns = new ArrayList<>();
            columnMap = new int[allColumns.length];
            for (int i = 0; i < allColumns.length; i++) {
                ShellFolderColumnInfo column = allColumns[i];
                if (column.isVisible()) {
                    columnMap[visibleColumns.size()] = i;
                    visibleColumns.add(column);
                }
            }

            columns = new ShellFolderColumnInfo[visibleColumns.size()];
            visibleColumns.toArray(columns);
            columnMap = Arrays.copyOf(columnMap, columns.length);

            List<? extends RowSorter.SortKey> sortKeys = (rowSorter == null) ? null : rowSorter.getSortKeys();
            fireTableStructureChanged();
            restoreSortKeys(sortKeys);
        }

        protected void restoreSortKeys(List<? extends RowSorter.SortKey> sortKeys) {
            if (sortKeys != null) {
                // check if preserved sortKeys are valid for this folder
                for (int i = 0; i < sortKeys.size(); i++) {
                    RowSorter.SortKey sortKey = sortKeys.get(i);
                    if (sortKey.getColumn() >= columns.length) {
                        sortKeys = null;
                        break;
                    }
                }
                if (sortKeys != null) {
                    rowSorter.setSortKeys(sortKeys);
                }
            }
        }

        public int getRowCount() {
            return directoryModel.getSize();
        }

        public int getColumnCount() {
            return columns.length;
        }

        public Object getValueAt(final int row, final int col) {
            // Note: It is very important to avoid getting info on drives, as
            // this will trigger "No disk in A:" and similar dialogs.
            //
            // Use (f.exists() && !chooser.getFileSystemView().isFileSystemRoot(f)) to
            // determine if it is safe to call methods directly on f.
            return getFileColumnValue((File) directoryModel.getElementAt(row), col);
        }

        public boolean isCellEditable(final int row, final int column) {
            File currentDirectory = getFileChooser().getCurrentDirectory();
            return (!readOnly && column == COLUMN_FILENAME && canWrite(currentDirectory, getFileChooser()));
        }

        public void setValueAt(final Object value, final int row, final int col) {
            if (col == COLUMN_FILENAME) {
                final JFileChooser chooser = getFileChooser();
                File f = (File) getValueAt(row, col);
                if (f != null) {
                    String oldDisplayName = chooser.getName(f);
                    String oldFileName = f.getName();
                    String newDisplayName = ((String) value).trim();
                    String newFileName;

                    if (!newDisplayName.equals(oldDisplayName)) {
                        newFileName = newDisplayName;
                        // Check if extension is hidden from user
                        int i1 = oldFileName.length();
                        int i2 = oldDisplayName.length();
                        if (i1 > i2 && oldFileName.charAt(i2) == '.') {
                            newFileName = newDisplayName + oldFileName.substring(i2);
                        }

                        // rename
                        FileSystemView fsv = chooser.getFileSystemView();
                        final File f2 = fsv.createFileObject(f.getParentFile(), newFileName);
                        if (f2.exists()) {
                            JOptionPane.showMessageDialog(chooser,
                                    MessageFormat.format(renameErrorFileExistsText, oldFileName), renameErrorTitleText,
                                    JOptionPane.ERROR_MESSAGE);
                        } else {
                            if (DarkFilePaneUIBridge.this.getModel().renameFile(f, f2)) {
                                if (fsv.isParent(chooser.getCurrentDirectory(), f2)) {
                                    // The setSelectedFile method produces a new setValueAt invocation while the
                                    // JTable
                                    // is editing. Postpone file selection to be sure that edit mode of the JTable
                                    // is completed
                                    SwingUtilities.invokeLater(() -> {
                                        if (chooser.isMultiSelectionEnabled()) {
                                            chooser.setSelectedFiles(new File[] {f2});
                                        } else {
                                            chooser.setSelectedFile(f2);
                                        }
                                    });
                                } else {
                                    // Could be because of delay in updating Desktop folder
                                    // chooser.setSelectedFile(null);
                                }
                            } else {
                                JOptionPane.showMessageDialog(chooser,
                                        MessageFormat.format(renameErrorText, oldFileName), renameErrorTitleText,
                                        JOptionPane.ERROR_MESSAGE);
                            }
                        }
                    }
                }
            }
        }

        protected Object getFileColumnValue(final File f, final int col) {
            if (col == COLUMN_SIZE) {
                return f.isDirectory() ? null : f.length();
            }
            return (col == COLUMN_FILENAME) ? f // always return the file itself for the 1st column
                    : ShellFolder.getFolderColumnValue(f, columnMap[col]);
        }

        public void intervalAdded(final ListDataEvent e) {
            int i0 = e.getIndex0();
            int i1 = e.getIndex1();
            if (i0 == i1) {
                File file = (File) getModel().getElementAt(i0);
                if (file.equals(newFolderFile)) {
                    new DelayedSelectionUpdater(file);
                    newFolderFile = null;
                }
            }

            fireTableRowsInserted(e.getIndex0(), e.getIndex1());
        }

        public void intervalRemoved(final ListDataEvent e) {
            fireTableRowsDeleted(e.getIndex0(), e.getIndex1());
        }

        public void contentsChanged(final ListDataEvent e) {
            // Update the selection after the model has been updated
            new DelayedSelectionUpdater();
            fireTableDataChanged();
        }

        public ShellFolderColumnInfo[] getColumns() {
            return columns;
        }
    }

    protected class DetailsTableRowSorter extends TableRowSorter<TableModel> {
        public DetailsTableRowSorter() {
            SorterModelWrapper modelWrapper = new SorterModelWrapper();
            setModelWrapper(modelWrapper);
            modelWrapper.getModel().addTableModelListener(e -> modelStructureChanged());
        }

        public void updateComparators(final ShellFolderColumnInfo[] columns) {
            for (int i = 0; i < columns.length; i++) {
                Comparator<?> c = columns[i].getComparator();
                if (c != null) {
                    c = new DirectoriesFirstComparatorWrapper(i, c);
                }
                setComparator(i, c);
            }
        }

        @Override
        public void sort() {
            ShellFolder.invoke((Callable<Void>) () -> {
                DetailsTableRowSorter.super.sort();
                return null;
            });
        }

        public void modelStructureChanged() {
            super.modelStructureChanged();
            updateComparators(detailsTableModel.getColumns());
        }

        protected class SorterModelWrapper extends ModelWrapper<TableModel, Integer> {
            public TableModel getModel() {
                return getDetailsTableModel();
            }

            public int getColumnCount() {
                return getDetailsTableModel().getColumnCount();
            }

            public int getRowCount() {
                return getDetailsTableModel().getRowCount();
            }

            public Object getValueAt(final int row, final int column) {
                return DarkFilePaneUIBridge.this.getModel().getElementAt(row);
            }

            public Integer getIdentifier(final int row) {
                return row;
            }
        }
    }

    /**
     * This class sorts directories before files, comparing directory to directory and file to file
     * using the wrapped comparator.
     */
    protected class DirectoriesFirstComparatorWrapper implements Comparator<File> {
        protected final Comparator<Object> comparator;
        protected final int column;

        @SuppressWarnings("unchecked")
        public DirectoriesFirstComparatorWrapper(final int column, final Comparator<?> comparator) {
            this.column = column;
            this.comparator = (Comparator<Object>) comparator;
        }

        public int compare(final File f1, final File f2) {
            if (f1 != null && f2 != null) {
                boolean traversable1 = getFileChooser().isTraversable(f1);
                boolean traversable2 = getFileChooser().isTraversable(f2);
                // directories go first
                if (traversable1 && !traversable2) {
                    return -1;
                }
                if (!traversable1 && traversable2) {
                    return 1;
                }
            }
            if (detailsTableModel.getColumns()[column].isCompareByColumn()) {
                return comparator.compare(getDetailsTableModel().getFileColumnValue(f1, column),
                        getDetailsTableModel().getFileColumnValue(f2, column));
            }
            // For this column we need to pass the file itself (not a
            // column value) to the comparator
            return comparator.compare(f1, f2);
        }
    }

    @SuppressWarnings("serial")
    // JDK-implementation class
    public class DetailsTableCellRenderer extends DarkTableCellRenderer {
        final JFileChooser chooser;
        final DateFormat df;

        public DetailsTableCellRenderer(final JFileChooser chooser) {
            this.chooser = chooser;
            df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, chooser.getLocale());
        }

        public Component getTableCellRendererComponent(final JTable table, final Object value, boolean isSelected,
                final boolean hasFocus, final int row, final int column) {

            if ((table.convertColumnIndexToModel(column) != COLUMN_FILENAME
                    || (listViewWindowsStyle && !table.isFocusOwner())) && !fullRowSelection) {
                isSelected = false;
            }

            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            setIcon(null);
            int modelColumn = table.convertColumnIndexToModel(column);
            ShellFolderColumnInfo columnInfo = detailsTableModel.getColumns()[modelColumn];

            Integer alignment = columnInfo.getAlignment();
            if (alignment == null) {
                alignment = (value instanceof Number) ? SwingConstants.RIGHT : SwingConstants.LEADING;
            }

            setHorizontalAlignment(alignment);

            // formatting cell text
            // TUDU: it's rather a temporary trick, to be revised
            String text;
            if (value == null) {
                text = "";
            } else if (value instanceof File) {
                File file = (File) value;
                text = chooser.getName(file);
                Icon icon = chooser.getIcon(file);
                setIcon(icon);

            } else if (value instanceof Long) {
                long len = ((Long) value) / 1024L;
                if (listViewWindowsStyle) {
                    text = MessageFormat.format(kiloByteString, len + 1);
                } else if (len < 1024L) {
                    text = MessageFormat.format(kiloByteString, (len == 0L) ? 1L : len);
                } else {
                    len /= 1024L;
                    if (len < 1024L) {
                        text = MessageFormat.format(megaByteString, len);
                    } else {
                        len /= 1024L;
                        text = MessageFormat.format(gigaByteString, len);
                    }
                }

            } else if (value instanceof Date) {
                text = df.format((Date) value);

            } else {
                text = value.toString();
            }

            setText(text);

            return this;
        }
    }

    protected class AlignableTableHeaderRenderer implements TableCellRenderer {
        final TableCellRenderer wrappedRenderer;

        public AlignableTableHeaderRenderer(final TableCellRenderer wrappedRenderer) {
            this.wrappedRenderer = wrappedRenderer;
        }

        public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected,
                final boolean hasFocus, final int row, final int column) {

            Component c =
                    wrappedRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            int modelColumn = table.convertColumnIndexToModel(column);
            ShellFolderColumnInfo columnInfo = detailsTableModel.getColumns()[modelColumn];

            Integer alignment = columnInfo.getAlignment();
            if (alignment == null) {
                alignment = SwingConstants.CENTER;
            }
            if (c instanceof JLabel) {
                ((JLabel) c).setHorizontalAlignment(alignment);
            }

            return c;
        }
    }

    protected class DelayedSelectionUpdater implements Runnable {
        File editFile;

        DelayedSelectionUpdater() {
            this(null);
        }

        DelayedSelectionUpdater(final File editFile) {
            this.editFile = editFile;
            if (isShowing()) {
                SwingUtilities.invokeLater(this);
            }
        }

        public void run() {
            setFileSelected();
            if (editFile != null) {
                editFileName(getRowSorter().convertRowIndexToView(getModel().indexOf(editFile)));
                editFile = null;
            }
        }
    }

    protected class EditActionListener implements ActionListener {
        public void actionPerformed(final ActionEvent e) {
            applyEdit();
        }
    }

    protected abstract class Handler implements MouseListener {
        protected MouseListener doubleClickListener;

        public void mousePressed(final MouseEvent evt) {
            if (evt.getSource() instanceof JList) {
                // Forward event to Basic
                if (getDoubleClickListener() != null) {
                    getDoubleClickListener().mousePressed(evt);
                }
            }
        }

        public void mouseReleased(final MouseEvent evt) {
            if (evt.getSource() instanceof JList) {
                // Forward event to Basic
                if (getDoubleClickListener() != null) {
                    getDoubleClickListener().mouseReleased(evt);
                }
            }
        }

        public void mouseEntered(final MouseEvent evt) {
            JComponent source = (JComponent) evt.getSource();
            if (source instanceof JTable) {
                JTable table = (JTable) evt.getSource();

                TransferHandler th1 = getFileChooser().getTransferHandler();
                TransferHandler th2 = table.getTransferHandler();
                if (th1 != th2) {
                    table.setTransferHandler(th1);
                }

                boolean dragEnabled = getFileChooser().getDragEnabled();
                if (dragEnabled != table.getDragEnabled()) {
                    table.setDragEnabled(dragEnabled);
                }
            } else if (source instanceof JList) {
                // Forward event to Basic
                if (getDoubleClickListener() != null) {
                    getDoubleClickListener().mouseEntered(evt);
                }
            }
        }

        public void mouseExited(final MouseEvent evt) {
            if (evt.getSource() instanceof JList) {
                // Forward event to Basic
                if (getDoubleClickListener() != null) {
                    getDoubleClickListener().mouseExited(evt);
                }
            }
        }

        protected MouseListener getDoubleClickListener() {

            // Lazy creation of Basic's listener
            if (doubleClickListener == null && list != null) {
                doubleClickListener = fileChooserUIAccessor.createDoubleClickListener(list);
            }
            return doubleClickListener;
        }
    }
}
