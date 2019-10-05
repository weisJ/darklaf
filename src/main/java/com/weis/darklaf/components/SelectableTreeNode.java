package com.weis.darklaf.components;

import javax.swing.tree.DefaultMutableTreeNode;

public class SelectableTreeNode extends DefaultMutableTreeNode {

    private String label;

    public SelectableTreeNode() {
        this(null, false);
    }

    public SelectableTreeNode(final String label, final boolean isSelected) {
        this(label, isSelected, true);
    }

    public SelectableTreeNode(final String label, final boolean isSelected, final boolean allowsChildren) {
        super();
        parent = null;
        this.allowsChildren = allowsChildren;
        this.userObject = isSelected;
        this.label = label;
    }

    public void setSelected(final boolean selected) {
        this.userObject = selected;
    }

    public void setLabel(final String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }
}
