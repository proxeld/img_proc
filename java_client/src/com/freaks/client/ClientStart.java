package com.freaks.client;

/**
 * Created by proxeld on 09.01.15.
 */
public class ClientStart {

    public static void main(String[] argv) {

        // View.changeWindowStyle();
        final Controller controller = new Controller();
        View view = new View(controller);
    }
}
