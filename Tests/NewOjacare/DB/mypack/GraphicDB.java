package mypack;

import javax.swing.*;
import java.awt.*;

public class GraphicDB extends JFrame {

    public GraphicDB (String title, String [] fields, String[][] data) {
	super();
	setDefaultCloseOperation(EXIT_ON_CLOSE);
	setTitle(title);
 
	JTable tableau = new JTable(data, fields);
	
	getContentPane().add(tableau.getTableHeader(), BorderLayout.NORTH);
	getContentPane().add(tableau, BorderLayout.CENTER);
	
	pack();
	setVisible(true);
    }
}