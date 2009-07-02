package com.rontologist.solium.romannumerals.server.numeral.roman;

public class IX extends AbstractRomanNumeral {
    private static final IX INSTANCE = new IX();
    private static final int VALUE = 9;

    private IX() { }

    public static IX getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}