import com.bulenkov.darcula.ui.DarculaTreeUI;
import com.weis.darklaf.LafManager;
import com.weis.darklaf.components.SelectableTreeNode;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

public class TreeDemo extends JFrame {

    public TreeDemo() {
        super("JTree Demo");

        DefaultMutableTreeNode root = new DefaultMutableTreeNode("States");
        DefaultMutableTreeNode parent1 = new DefaultMutableTreeNode("Andhra Pradesh");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode("Vijayawada");
        DefaultMutableTreeNode child1 = new SelectableTreeNode("This node can be selected", true);
        DefaultMutableTreeNode parent2 = new DefaultMutableTreeNode("Telangana");
        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Hyderabad");

        // Adding child nodes to parent
        parent1.add(child);
        parent1.add(child1);
        parent2.add(child2);

        // Adding parent nodes to root
        root.add(parent1);
        root.add(parent2);

        // Adding root to JTree
        JTree tree = new JTree(root);
        tree.setEditable(true);

        getContentPane().add(new JScrollPane(tree));
        setSize(300, 300);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    public static void main(final String args[]) {
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);
            new TreeDemo();
        });
    }
}
