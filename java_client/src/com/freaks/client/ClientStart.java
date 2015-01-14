package com.freaks.client;

public class ClientStart {

    public static void main(String[] argv) {

        View.changeWindowStyle();
        final Controller controller = new Controller();
        View view = new View(controller);
    }
}
