package com.freaks.client;

import com.freaks.Settings;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.FontUIResource;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.TimerTask;
import java.util.logging.Logger;
import java.util.Timer;

public class View extends JFrame {
    private Controller controller;
    private BufferedImage sourceImage;
    private BufferedImage processedImage;
    private Timer progressBarTimer;
    private Logger guiLoger = Logger.getLogger("guiLogger");

    private JPanel rootPanel;
    private JButton averageButton;
    private JButton minButton;
    private JButton gaussButton;
    private JPanel sourceImagePanel;
    private JPanel resultImagePanel;
    private JButton connectButton;
    private JButton medianButton;
    private JPanel toolbarPanel;
    private JToolBar toolbar;
    private JButton erodeButton;
    private JButton dilateButton;
    private JButton openButton;
    private JButton closeButton;
    private JButton tophatButton;
    private JButton bottomhatButton;
    private JButton maxButton;
    private JButton prewittButton;
    private JButton sobelButton;
    private JPanel buttonsPanel;
    private JButton robertsButton;
    private JButton disconnectButton;
    private JProgressBar progressBar;

    private JMenuBar menuBar;
    private JMenu fileMenu;
    private JMenu helpMenu;
    private JMenuItem openFileMenuItem;
    private JMenuItem saveFileMenuItem;
    private JMenuItem aboutMenuItem;
    private JFileChooser fileChooser;
    private JPanel loadingPanel;


    /**
     * Main view creation
     */
    public View(Controller _controller) {
        super("Image processing");

        setContentPane(rootPanel);

        customCreation();
        bindEvents();

        pack();
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        controller = _controller;
        controller.setView(this);
        setVisible(true);
    }

    private void customCreation() {

        // fileMenu bar creation
        menuBar = new JMenuBar();
        fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);

        openFileMenuItem = new JMenuItem("Load image...", new ImageIcon("assets/open.gif"));
        openButton.setMnemonic(KeyEvent.VK_L);
        fileMenu.add(openFileMenuItem);

        saveFileMenuItem = new JMenuItem("Save image", new ImageIcon("assets/save.gif"));
        saveFileMenuItem.setMnemonic(KeyEvent.VK_S);
        fileMenu.add(saveFileMenuItem);

        helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);

        aboutMenuItem = new JMenuItem("About", KeyEvent.VK_A);
        helpMenu.add(aboutMenuItem);

        menuBar.add(fileMenu);
        menuBar.add(helpMenu);
        setJMenuBar(menuBar);

        // file chooser
        fileChooser = new JFileChooser();

        FileFilter ff = new FileFilter() {
            @Override
            public boolean accept(File file) {
                if (file.isDirectory()) {
                    return true;
                }

                String trimmed = file.getName().trim();
                String extension = trimmed.substring(trimmed.lastIndexOf(".")+1).toLowerCase();
                return extension.equals("png") || extension.equals("jpg") || extension.equals("jpeg") || extension.equals("bmp") ||
                        extension.equals("gif");
            }

            @Override
            public String getDescription() {
                return "Images";
            }
        };

        fileChooser.setAcceptAllFileFilterUsed(false);
        fileChooser.setFileFilter(ff);

        // loading pane;
        ImageIcon loading = new ImageIcon("assets/ajax-loader.gif");
        JLabel overlay = new JLabel("Processing...", loading, JLabel.CENTER);
        overlay.setVerticalTextPosition(JLabel.BOTTOM);
        overlay.setHorizontalTextPosition(JLabel.CENTER);
        loadingPanel =  new JPanel() {
            @Override
            public void paintComponent(Graphics g) {
                g.setColor(new Color(64, 64, 64, 128));
                g.fillRect(0, 0, View.this.getWidth(), View.this.getHeight());
            }
        };
        loadingPanel.setLayout(new BorderLayout());
        loadingPanel.add(overlay);
        this.setGlassPane(loadingPanel);

        // progress bar
        progressBarTimer = new Timer();
    }

    private void bindEvents() {

        openFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                int result = fileChooser.showDialog(View.this, "Choose image");

                if (result == JFileChooser.APPROVE_OPTION) {
                    File image = fileChooser.getSelectedFile();

                    try {
                        BufferedImage myPicture = ImageIO.read(image);

                        if(myPicture.getWidth() > 512 || myPicture.getHeight() > 512) {
                            showInfoPopup("Image max size is 512x512");
                            return;
                        }
                        JLabel picLabel = new JLabel(new ImageIcon(myPicture));
                        picLabel.setPreferredSize(new Dimension(512, 512));
                        picLabel.setSize(512, 512);
                        sourceImage = myPicture;
                        sourceImagePanel.removeAll();
                        sourceImagePanel.add(picLabel);
                        sourceImagePanel.repaint();
                        sourceImagePanel.revalidate();

                    } catch(IOException e) {
                        e.printStackTrace();
                    }

                    guiLoger.info("Opening file: " + image.getName());
                }
            }
        });

        saveFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                int result = fileChooser.showSaveDialog(View.this);

                if (result == JFileChooser.APPROVE_OPTION) {

                    File imageFile = fileChooser.getSelectedFile();
                    guiLoger.info("Saving file: " + imageFile.getName());
                    controller.onSave(processedImage, imageFile);
                }
            }
        });

        aboutMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                showInfoPopup("Authors: Maciej Urbanek & Jakub Pelczar\nVersion: " + Settings.VERSION);
            }
        });

        connectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onConnect();
                    }
                }).start();
            }
        });

        disconnectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onDisconnect();
                    }
                }).start();
            }
        });

        averageButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "filterAverage");

                        // according to best practices - invoke from event dispatch thread
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        gaussButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "filterGauss");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        medianButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "filterMedian");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        minButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "filterMin");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        maxButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "filterMax");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        erodeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "erode");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        dilateButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "dilate");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        openButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "open");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        closeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "close");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        tophatButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "tophat");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        bottomhatButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "bothat");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });
        prewittButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "prewitt");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        robertsButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "roberts");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });

        sobelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                setOperationsEnabled(false);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        controller.onOperationChosen(sourceImage, "sobel");
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                setOperationsEnabled(true);
                                hideLoadingPanel();
                            }
                        });
                    }
                }).start();
            }
        });
    }

    private void createUIComponents() {
        sourceImagePanel = new JPanel();
        resultImagePanel = new JPanel();
        connectButton = new JButton("", new ImageIcon("assets/connect.gif"));
        disconnectButton = new JButton("", new ImageIcon("assets/disconnect.gif"));
    }

    public void setProcessedImage(ImageIcon imageIcon) {

        BufferedImage bi = new BufferedImage(
                imageIcon.getIconWidth(),
                imageIcon.getIconHeight(),
                BufferedImage.TYPE_BYTE_GRAY);
        Graphics g = bi.createGraphics();
        imageIcon.paintIcon(null, g, 0, 0);
        g.dispose();

        processedImage = bi;
    }

    public void displayProcessedImage(ImageIcon imageIcon) {
        JLabel picLabel = new JLabel(imageIcon);

        picLabel.setPreferredSize(new Dimension(512, 512));
        picLabel.setSize(512, 512);

        resultImagePanel.removeAll();
        resultImagePanel.add(picLabel);
        resultImagePanel.repaint();
        resultImagePanel.revalidate();
    }

    public void setOperationsEnabled(boolean enabled) {
        maxButton.setEnabled(enabled);
        minButton.setEnabled(enabled);
        averageButton.setEnabled(enabled);
        gaussButton.setEnabled(enabled);
        medianButton.setEnabled(enabled);
        openButton.setEnabled(enabled);
        closeButton.setEnabled(enabled);
        erodeButton.setEnabled(enabled);
        dilateButton.setEnabled(enabled);
        bottomhatButton.setEnabled(enabled);
        tophatButton.setEnabled(enabled);
        prewittButton.setEnabled(enabled);
        robertsButton.setEnabled(enabled);
        sobelButton.setEnabled(enabled);
    }

    public void showLoadingPanel() {
        getGlassPane().setVisible(true);
    }

    public void hideLoadingPanel() {
        getGlassPane().setVisible(false);
    }

    public void showInfoPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg);
    }

    public void showWarningPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg, "Warning", JOptionPane.WARNING_MESSAGE);
    }

    public void showErrorPopup(String msg) {
        JOptionPane.showMessageDialog(View.this, msg, "Something went wrong...", JOptionPane.ERROR_MESSAGE);
    }

    public void fillConnectProgressBar() {
        progressBarTimer = new Timer();
        progressBar.setValue(0);

        progressBarTimer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                progressBar.setValue(progressBar.getValue() + 1);
                if (progressBar.getValue() > 100)
                    this.cancel();
            }
        }, 0, 30);
    }

    public void fill100ProgressBar() {
        progressBarTimer.cancel();
        progressBar.setValue(100);
    }

    public void progressBarSetVisible(boolean visible) {

        progressBar.setVisible(visible);
    }

    public void setController(Controller _controller) {
        controller = _controller;
    }

    /**
     * Completely changes style of the elements building the UI
     */
    public static void changeWindowStyle() {

        UIManager.put("nimbusBase", new Color(0x3C3F41));
        UIManager.put("nimbusBlueGrey", new Color(0x494E50));
        UIManager.put("control", new Color(0x3C3F41));
        UIManager.put("text", new Color(0x909090));
        UIManager.put("ComboBox.background", new ColorUIResource(UIManager.getColor("TextField.background")));
        UIManager.put("ComboBox.foreground", new ColorUIResource(UIManager.getColor("TextField.foreground")));
        UIManager.put("ComboBox.selectionBackground", new ColorUIResource(Color.GREEN));
        UIManager.put("TabbedPane:TabbedPaneTab[Disabled].backgroundPainter", new ColorUIResource(Color.WHITE));
        UIManager.put("Button.foreground", new Color(0, 0, 0));

        /**
         * Changing appearance style
         */
        for (UIManager.LookAndFeelInfo laf : UIManager.getInstalledLookAndFeels()) {
            if ("Nimbus".equals(laf.getName())){
                try {
                    UIManager.setLookAndFeel(laf.getClassName());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        // changing fonts
        Font font = new Font("Droid Sans", Font.BOLD, 13);
        Enumeration keys = UIManager.getDefaults().keys();
        while (keys.hasMoreElements()) {
            Object key = keys.nextElement();
            Object value = UIManager.get (key);
            if (value != null && value instanceof FontUIResource)
                UIManager.put (key, font);
        }

        UIManager.put("nimbusLightBackground", Color.BLACK);
        UIManager.put("Label.font", font);

    }
}
