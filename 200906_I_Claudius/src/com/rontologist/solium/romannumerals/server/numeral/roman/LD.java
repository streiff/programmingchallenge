package com.rontologist.solium.romannumerals.server.numeral.roman;

public class LD extends AbstractRomanNumeral {
    private static final LD INSTANCE = new LD();
    private static final int VALUE = 450;

    private LD() { }

    public static LD getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}