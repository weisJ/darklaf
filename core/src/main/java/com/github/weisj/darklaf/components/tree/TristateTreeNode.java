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
package com.github.weisj.darklaf.components.tree;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;

import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class TristateTreeNode extends DefaultMutableTreeNode implements LabeledTreeNode {

    private String label;

    public TristateTreeNode() {
        this(null);
    }

    public TristateTreeNode(final String label) {
        this(label, TristateState.DESELECTED);
    }

    public TristateTreeNode(final String label, final TristateState state) {
        this(label, state, true);
    }

    public TristateTreeNode(final String label, final TristateState state, final boolean allowsChildren) {
        super();
        parent = null;
        this.allowsChildren = allowsChildren;
        this.userObject = state;
        this.label = label;
    }

    @Override
    public void add(final MutableTreeNode newChild) {
        if (!(newChild instanceof TristateTreeNode)) {
            throw new IllegalArgumentException("Only children of type TristateTreeNode are allowed.");
        }
        super.add(newChild);
    }

    public TristateState getState() {
        return getUserObject();
    }

    public void setState(final TristateState state) {
        setState(state, false, false);
    }

    private void setState(final TristateState s, final boolean invokedByParent, final boolean invokedByChild) {
        if (isLeaf() && ((TristateState) userObject).isIndeterminate()) {
            throw new IllegalArgumentException("Leaf nodes cannot have an indeterminate state");
        }
        TristateState state = s.isIndeterminate() && !getState().isIndeterminate() ? getState().next().next() : s;
        super.setUserObject(state);
        if (!isLeaf() && !invokedByChild) {
            if (!state.isIndeterminate()) {
                for (Object node : children) {
                    if (node instanceof TristateTreeNode) {
                        ((TristateTreeNode) node).setState(state, true, false);
                    }
                }
            }
        }
        if (!invokedByParent) {
            TristateTreeNode treeNode = DarkUIUtil.nullableCast(TristateTreeNode.class, getParent());
            if (treeNode != null) {
                treeNode.setState(treeNode.getEffectiveState(), false, true);
            }
        }
    }

    public void setSelected(final boolean selected) {
        this.userObject = selected;
    }

    @Override
    public TristateState getUserObject() {
        return DarkUIUtil.nullableCast(TristateState.class, super.getUserObject());
    }

    @Override
    public void setUserObject(final Object userObject) {
        if (!(userObject instanceof TristateState)) {
            throw new IllegalArgumentException("Only values of type TristateState are allowed but got " + userObject);
        }
        setState((TristateState) userObject);
    }

    public TristateState getEffectiveState() {
        TristateState state = null;
        for (Object node : children) {
            if (node instanceof TristateTreeNode) {
                TristateState nodeState = ((TristateTreeNode) node).getUserObject();
                if (state == null) state = nodeState;
                if (state != nodeState) {
                    state = TristateState.INDETERMINATE_SEL;
                    break;
                }
            }
        }
        return state != null ? state : TristateState.DESELECTED;
    }

    @Override
    public String getLabel() {
        return label;
    }

    public void setLabel(final String label) {
        this.label = label;
    }
}
