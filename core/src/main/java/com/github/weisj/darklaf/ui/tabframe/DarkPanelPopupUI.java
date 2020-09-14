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
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.FocusManager;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.components.border.MutableLineBorder;
import com.github.weisj.darklaf.components.tabframe.*;
import com.github.weisj.darklaf.components.uiresource.JLabelUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.panel.DarkPanelUI;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.ResourceUtil;

public class DarkPanelPopupUI extends DarkPanelUI implements PropertyChangeListener, AWTEventListener, TabFramePopupUI {

    protected HeaderButton closeButton;
    private final Action closeAction = new AbstractAction() {
        @Override
        public void actionPerformed(final ActionEvent e) {
            closeButton.doClick();
        }
    };
    protected JPanel content;
    protected JLabel label;
    protected Color headerFocusBackground;
    protected Color headerButtonFocusHoverBackground;
    protected Color headerButtonFocusClickBackground;
    protected Color headerBackground;
    protected Color headerButtonHoverBackground;
    protected Color headerButtonClickBackground;
    protected String accelerator;
    private PanelPopup popupComponent;
    private JPanel header;
    private MutableLineBorder headerBorder;
    private MutableLineBorder contentBorder;
    private boolean oldFocus;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkPanelPopupUI();
    }

    @Override
    public void installUI(final JComponent c) {
        popupComponent = (PanelPopup) c;
        super.installUI(c);
        installDefaults();
        installComponents();
        installListeners();
        updateBorder(false);
    }

    protected void installDefaults() {
        headerBackground = UIManager.getColor("TabFramePopup.headerBackground");
        headerButtonHoverBackground = UIManager.getColor("TabFramePopup.headerButtonHoverBackground");
        headerButtonClickBackground = UIManager.getColor("TabFramePopup.headerButtonClickBackground");
        headerFocusBackground = UIManager.getColor("TabFramePopup.headerFocusBackground");
        headerButtonFocusHoverBackground = UIManager.getColor("TabFramePopup.headerButtonFocusHoverBackground");
        headerButtonFocusClickBackground = UIManager.getColor("TabFramePopup.headerButtonFocusClickBackground");
        accelerator = UIManager.getString("TabFramePopup.closeAccelerator");
        popupComponent.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                .put(KeyStroke.getKeyStroke(accelerator), accelerator);
        popupComponent.getActionMap().put(accelerator, closeAction);
        popupComponent.setLayout(new BorderLayout());
    }

    protected void installComponents() {
        closeButton = createCloseButton();
        label = createLabel();
        header = createHeader();

        JPanel headerContainer = new JPanel(new BorderLayout());
        setHeaderBackground(false);
        headerContainer.add(header, BorderLayout.CENTER);
        headerContainer.setMinimumSize(UIManager.getDimension("TabFramePopup.minimumHeaderSize"));

        content = new JPanel(new BorderLayout());
        content.add(popupComponent.getContentPane(), BorderLayout.CENTER);

        popupComponent.setLayout(new BorderLayout());
        popupComponent.add(headerContainer, BorderLayout.NORTH);
        popupComponent.add(content, BorderLayout.CENTER);

        headerBorder = createBorder();
        contentBorder = createBorder();
        headerContainer.setBorder(headerBorder);
        content.setBorder(contentBorder);
    }

    protected void installListeners() {
        popupComponent.addPropertyChangeListener(this);
        Toolkit.getDefaultToolkit().addAWTEventListener(this, AWTEvent.FOCUS_EVENT_MASK);
        JTabFrame frame = popupComponent.getTabFrame();
        if (frame != null) {
            frame.addPropertyChangeListener(this);
        }
    }

    protected HeaderButton createCloseButton() {
        HeaderButton closeButton = new HeaderButton(UIManager.getIcon("TabFramePopup.close.icon"), this);
        closeButton.setBorder(new EmptyBorder(4, 4, 4, 4));
        closeButton.addActionListener(e -> popupComponent.close());
        String tooltip = ResourceUtil.getResourceBundle("actions", popupComponent).getString("Actions.close");
        tooltip = tooltip + " (" + UIManager.getString("TabFramePopup.closeTooltipTextHint") + ")";
        closeButton.setToolTipText(tooltip);
        return closeButton;
    }

    protected JLabel createLabel() {
        JLabelUIResource label = new JLabelUIResource(popupComponent.getTitle(), popupComponent.getIcon(), JLabel.LEFT);
        label.setOpaque(false);
        return label;
    }

    protected JPanel createHeader() {
        JPanel header = new JPanel();
        header.setLayout(new BoxLayout(header, BoxLayout.X_AXIS));
        header.add(Box.createHorizontalStrut(5));
        header.add(label);
        header.add(Box.createGlue());
        header.add(closeButton);
        header.add(Box.createHorizontalStrut(1));
        header.setBorder(UIManager.getBorder("TabFramePopup.headerBorder"));
        return header;
    }

    protected void setHeaderBackground(final boolean focus) {
        closeButton.setFocus(focus);
        if (header != null) {
            header.setBackground(focus ? headerFocusBackground : headerBackground);
        }
        if (oldFocus != focus) {
            if (header != null) {
                header.repaint();
            }
            closeButton.repaint();
            oldFocus = focus;
        }
    }

    protected MutableLineBorder createBorder() {
        Color color = UIManager.getColor("TabFramePopup.borderColor");
        return new MutableLineBorder(0, 0, 0, 0, color);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallComponents();
        uninstallListeners();
        popupComponent.removeAll();
        popupComponent.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                .remove(KeyStroke.getKeyStroke(accelerator));
        popupComponent.getActionMap().remove(accelerator);
        popupComponent = null;
    }

    protected void uninstallComponents() {
        popupComponent.removeAll();
    }

    protected void uninstallListeners() {
        popupComponent.removePropertyChangeListener(this);
        Toolkit.getDefaultToolkit().removeAWTEventListener(this);
        JTabFrame frame = popupComponent.getTabFrame();
        if (frame != null) {
            frame.removePropertyChangeListener(this);
        }
    }

    public final Dimension getPreferredSize(final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getPreferredSize(c);
        }
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getMinimumSize(c);
        }
    }

    @Override
    public Dimension getMaximumSize(final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getMaximumSize(c);
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (TabFramePopup.KEY_OPEN.equals(key)) {
            if (Boolean.TRUE.equals(evt.getNewValue())) {
                setHeaderBackground(true);
            }
        } else if (TabFramePopup.KEY_CONTENT.equals(key)) {
            if (content == null) return;
            content.add((Component) evt.getNewValue(), BorderLayout.CENTER);
            content.invalidate();
        } else if (TabFramePopup.KEY_TITLE.equals(key)) {
            if (label == null) return;
            label.setText(evt.getNewValue().toString());
            label.repaint();
        } else if (TabFramePopup.KEY_ICON.equals(key)) {
            if (label == null) return;
            label.setIcon((Icon) evt.getNewValue());
            label.repaint();
        } else if (TabFramePopup.KEY_VISIBLE_TAB.equals(key)) {
            if (evt.getNewValue() instanceof JTabFrame.TabFramePosition) {
                if (((JTabFrame.TabFramePosition) evt.getNewValue()).getAlignment() == popupComponent.getAlignment()) {
                    updateBorder(true);
                }
            }
        } else if (TabFramePopup.KEY_TAB_FRAME_PARENT.equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal instanceof JTabFrame) {
                ((JTabFrame) oldVal).removePropertyChangeListener(this);
            }
            if (newVal instanceof JTabFrame) {
                ((JTabFrame) newVal).addPropertyChangeListener(this);
            }
        } else if (TabFramePopup.KEY_PEER_INSETS.equals(key)) {
            updateBorder(false);
        }
    }

    protected void updateBorder(final boolean notifyPeer) {
        if (popupComponent.getTabFrame() != null) {
            JTabFrame tabFrame = popupComponent.getTabFrame();
            boolean[] status = tabFrame.getContentPane().getStatus();
            Alignment alignment = popupComponent.getAlignment();
            Insets insets = getBorderSize(tabFrame, alignment, status);

            applyBorderInsets(insets);
            Component component = popupComponent.getComponent();
            popupComponent.doLayout();
            popupComponent.repaint();
            if (component != null && component != popupComponent) {
                component.doLayout();
                component.repaint();
            }
            if (header != null) {
                Component headerParent = header.getParent();
                if (headerParent != null) {
                    headerParent.doLayout();
                    headerParent.repaint();
                }
            }

            if (notifyPeer) {
                try {
                    Component peer = tabFrame.getPopupComponentAt(tabFrame.getPeer(popupComponent.getAlignment()));
                    peer.firePropertyChange(TabFramePopup.KEY_PEER_INSETS, 0, 1);
                } catch (IndexOutOfBoundsException ignored) {
                    /* may happen during transfer */ }
            }
        }
    }

    protected Insets getBorderSize(final JTabFrame tabFrame, final Alignment a, final boolean[] info) {
        Insets insets = new Insets(0, 0, 0, 0);
        switch (a) {
            case NORTH:
            case NORTH_EAST:
                if (info[a.getIndex()] || info[tabFrame.getPeer(a).getIndex()]) {
                    insets.bottom = 1;
                }
                if (info[Alignment.NORTH.getIndex()] && a == Alignment.NORTH_EAST) {
                    insets.left = 1;
                }
                return insets;
            case SOUTH:
            case SOUTH_WEST:
                if (info[a.getIndex()] || info[tabFrame.getPeer(a).getIndex()]) {
                    insets.top = 1;
                }
                if (info[Alignment.SOUTH_WEST.getIndex()] && a == Alignment.SOUTH) {
                    insets.left = 1;
                }
                return insets;
            case EAST:
            case SOUTH_EAST:
                if (info[a.getIndex()] || info[tabFrame.getPeer(a).getIndex()]) {
                    insets.left = 1;
                }
                if (info[Alignment.EAST.getIndex()] && a == Alignment.SOUTH_EAST) {
                    insets.top = 1;
                }
                return insets;
            case WEST:
            case NORTH_WEST:
                if (info[a.getIndex()] || info[tabFrame.getPeer(a).getIndex()]) {
                    insets.right = 1;
                }
                if (info[Alignment.NORTH_WEST.getIndex()] && a == Alignment.WEST) {
                    insets.top = 1;
                }
                return insets;
            default:
                return insets;
        }
    }

    protected void applyBorderInsets(final Insets insets) {
        headerBorder.setInsets(insets.top, insets.left, 1, insets.right);
        contentBorder.setInsets(0, insets.left, insets.bottom, insets.right);
    }

    protected boolean hasFocus() {
        return oldFocus;
    }

    @Override
    public void eventDispatched(final AWTEvent event) {
        if (event.getID() == FocusEvent.FOCUS_GAINED) {
            Component focusOwner = FocusManager.getCurrentManager().getFocusOwner();
            if (focusOwner instanceof JTabFrame) return;
            if (focusOwner instanceof JRootPane) return;
            boolean focus = DarkUIUtil.hasFocus(popupComponent);
            if (popupComponent.getTabFrame() != null) {
                Container container =
                        popupComponent.getTabFrame().getContentPane().getContainer(popupComponent.getAlignment());
                focus = focus || DarkUIUtil.hasFocus(container);
            }
            setHeaderBackground(focus);
        }
    }

    protected static final class HeaderButton extends JButton implements UIResource {

        protected final DarkPanelPopupUI ui;

        public HeaderButton(final Icon icon, final DarkPanelPopupUI ui) {
            super(icon);
            this.ui = ui;
            putClientProperty(DarkButtonUI.KEY_SQUARE, true);
            putClientProperty(DarkButtonUI.KEY_THIN, true);
            putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS);
            putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
            setRolloverEnabled(true);
            Insets ins = UIManager.getInsets("TabFramePopup.headerButton.insets");
            setMargin(new Insets(ins.top, ins.left, ins.bottom, ins.right));
            setFocus(false);
            setFocusable(false);
            setOpaque(false);
        }

        public void setFocus(final boolean focus) {
            putClientProperty(DarkButtonUI.KEY_HOVER_COLOR,
                    focus ? ui.headerButtonFocusHoverBackground : ui.headerButtonHoverBackground);
            putClientProperty(DarkButtonUI.KEY_CLICK_COLOR,
                    focus ? ui.headerButtonFocusClickBackground : ui.headerButtonClickBackground);
        }
    }
}
