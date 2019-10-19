/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.tabframe;

import com.weis.darklaf.components.JLabelUIResource;
import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.border.MutableLineBorder;
import com.weis.darklaf.components.tabframe.PanelPopup;
import com.weis.darklaf.components.tabframe.TabFrame;
import com.weis.darklaf.components.tooltip.ToolTipContext;
import com.weis.darklaf.ui.panel.DarkPanelUI;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.FocusManager;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkPanelPopupUI extends DarkPanelUI implements PropertyChangeListener, AWTEventListener {


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

    @NotNull
    @Contract("_ -> new")
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
        var frame = popupComponent.getTabFrame();
        if (frame != null) {
            frame.addPropertyChangeListener(this);
        }
    }

    protected HeaderButton createCloseButton() {
        var closeButton = new HeaderButton(UIManager.getIcon("TabFramePopup.close.icon"), this);
        closeButton.setBorder(new EmptyBorder(4, 4, 4, 4));
        closeButton.addActionListener(e -> popupComponent.close());
        var tooltip = UIManager.getString("TabFramePopup.closeTooltipText");
        closeButton.setToolTipText(tooltip);
        return closeButton;
    }

    protected JLabel createLabel() {
        var label = new JLabelUIResource(popupComponent.getTitle(), popupComponent.getIcon(), JLabel.LEFT);
        label.setOpaque(false);
        return label;
    }

    protected JPanel createHeader() {
        var header = new JPanel();
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
        var color = UIManager.getColor("TabFramePopup.borderColor");
        return new MutableLineBorder.UIResource(0, 0, 0, 0, color);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallComponents();
        uninstallListeners();
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
        var frame = popupComponent.getTabFrame();
        if (frame != null) {
            frame.removePropertyChangeListener(this);
        }
    }

    public final Dimension getPreferredSize(@NotNull final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getPreferredSize(c);
        }
    }

    @Override
    public Dimension getMinimumSize(@NotNull final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getMinimumSize(c);
        }
    }

    @Override
    public Dimension getMaximumSize(@NotNull final JComponent c) {
        if (!c.isEnabled()) {
            return new Dimension(0, 0);
        } else {
            return super.getMaximumSize(c);
        }
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        var key = evt.getPropertyName();
        if ("open".equals(key)) {
            if (Boolean.TRUE.equals(evt.getNewValue())) {
                setHeaderBackground(true);
            }
        } else if ("content".equals(key)) {
            if (content == null) return;
            content.add((Component) evt.getNewValue(), BorderLayout.CENTER);
            content.invalidate();
        } else if ("title".equals(key)) {
            if (label == null) return;
            label.setText(evt.getNewValue().toString());
            label.repaint();
        } else if ("icon".equals(key)) {
            if (label == null) return;
            label.setIcon((Icon) evt.getNewValue());
            label.repaint();
        } else if ("visibleTab".equals(key)) {
            if (evt.getNewValue() instanceof TabFrame.TabFramePosition) {
                if (((TabFrame.TabFramePosition) evt.getNewValue()).getAlignment() == popupComponent.getAlignment()) {
                    updateBorder(true);
                }
            }
        } else if ("tabFrame".equals(key)) {
            var oldVal = evt.getOldValue();
            var newVal = evt.getNewValue();
            if (oldVal instanceof TabFrame) {
                ((TabFrame) oldVal).removePropertyChangeListener(this);
            }
            if (newVal instanceof TabFrame) {
                ((TabFrame) newVal).addPropertyChangeListener(this);
            }
        } else if ("peerInsets".equals(key)) {
            updateBorder(false);
            updateTooltip();
        } else if ("alignment".equals(key)) {
            updateTooltip();
        }
    }

    protected void updateTooltip() {
        var tabFrame = popupComponent.getTabFrame();
        closeButton.setAlignment(popupComponent.getAlignment(),
                                 tabFrame.getContentPane().isEnabled(tabFrame.getPeer(popupComponent.getAlignment())));
    }

    protected void updateBorder(final boolean notifyPeer) {
        if (popupComponent.getTabFrame() != null) {
            var tabFrame = popupComponent.getTabFrame();
            var status = tabFrame.getContentPane().getStatus();
            var alignment = popupComponent.getAlignment();
            var insets = getBorderSize(alignment, status);

            applyBorderInsets(insets);
            popupComponent.doLayout();
            popupComponent.repaint();
            if (notifyPeer) {
                var peer = tabFrame.getPopupComponentAt(tabFrame.getPeer(popupComponent.getAlignment()));
                peer.firePropertyChange("peerInsets", 0, 1);
            }
        }
    }

    @NotNull
    protected Insets getBorderSize(@NotNull final Alignment a, final boolean[] info) {
        var insets = new Insets(0, 0, 0, 0);
        switch (a) {
            case NORTH:
            case NORTH_EAST:
                if (info[Alignment.NORTH.getIndex()] || info[Alignment.NORTH_EAST.getIndex()]) {
                    insets.bottom = 1;
                }
                if (info[Alignment.NORTH.getIndex()] && a == Alignment.NORTH_EAST) {
                    insets.left = 1;
                }
                return insets;
            case SOUTH:
            case SOUTH_WEST:
                if (info[Alignment.SOUTH_WEST.getIndex()] || info[Alignment.SOUTH.getIndex()]) {
                    insets.top = 1;
                }
                if (info[Alignment.SOUTH_WEST.getIndex()] && a == Alignment.SOUTH) {
                    insets.left = 1;
                }
                return insets;
            case EAST:
            case SOUTH_EAST:
                if (info[Alignment.EAST.getIndex()] || info[Alignment.SOUTH_EAST.getIndex()]) {
                    insets.left = 1;
                }
                if (info[Alignment.EAST.getIndex()] && a == Alignment.SOUTH_EAST) {
                    insets.top = 1;
                }
                return insets;
            case WEST:
            case NORTH_WEST:
                if (info[Alignment.NORTH_WEST.getIndex()] || info[Alignment.WEST.getIndex()]) {
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

    protected void applyBorderInsets(@NotNull final Insets insets) {
        headerBorder.setInsets(insets.top, insets.left, 1, insets.right);
        contentBorder.setInsets(0, insets.left, insets.bottom, insets.right);
    }

    protected boolean hasFocus() {
        return oldFocus;
    }

    @Override
    public void eventDispatched(@NotNull final AWTEvent event) {
        if (event.getID() == FocusEvent.FOCUS_GAINED) {
            var focusOwner = FocusManager.getCurrentManager().getFocusOwner();
            if (focusOwner instanceof TabFrame) return;
            if (focusOwner instanceof JRootPane) return;
            boolean focus = DarkUIUtil.hasFocus(popupComponent);
            setHeaderBackground(focus);
        }
    }

    protected static final class HeaderButton extends JButton implements UIResource {

        protected final ToolTipContext context = new ToolTipContext(this);
        protected final DarkPanelPopupUI ui;

        public HeaderButton(@NotNull final Icon icon, final DarkPanelPopupUI ui) {
            super(icon);
            this.ui = ui;
            putClientProperty("JButton.buttonType", "square");
            putClientProperty("JButton.alternativeArc", Boolean.TRUE);
            putClientProperty("JButton.variant", "shadow");
            setRolloverEnabled(true);
            setMargin(new InsetsUIResource(2, 2, 2, 2));
            setFocus(false);
            setOpaque(false);
            context.setToolTipInsets(new Insets(5, 5, 5, 5));
        }

        public void setFocus(final boolean focus) {
            putClientProperty("JButton.shadow.hover", focus ? ui.headerButtonFocusHoverBackground
                                                            : ui.headerButtonHoverBackground);
            putClientProperty("JButton.shadow.click", focus ? ui.headerButtonFocusClickBackground
                                                            : ui.headerButtonClickBackground);
        }

        @Override
        public Point getToolTipLocation(final MouseEvent event) {
            return context.getToolTipLocation(event);
        }

        @Override
        public JToolTip createToolTip() {
            return context.getToolTip();
        }

        protected void setAlignment(@NotNull final Alignment a, final boolean peerEnabled) {
            switch (a) {
                case NORTH:
                    context.setCenterAlignment(peerEnabled ? Alignment.SOUTH : Alignment.SOUTH_WEST);
                    break;
                case NORTH_EAST:
                    context.setCenterAlignment(Alignment.SOUTH_WEST);
                    break;
                case EAST:
                    context.setCenterAlignment(Alignment.WEST);
                    break;
                case SOUTH_EAST:
                    context.setCenterAlignment(Alignment.WEST);
                    break;
                case SOUTH:
                    context.setCenterAlignment(Alignment.NORTH_WEST);
                    break;
                case SOUTH_WEST:
                    context.setCenterAlignment(peerEnabled ? Alignment.NORTH : Alignment.NORTH_WEST);
                    break;
                case WEST:
                    context.setCenterAlignment(peerEnabled ? Alignment.NORTH : Alignment.SOUTH);
                    break;
                case NORTH_WEST:
                    context.setCenterAlignment(Alignment.SOUTH);
                    break;
                case CENTER:
                    break;
            }
        }
    }
}
