package com.rontologist.solium.romannumerals.server.numeral.roman;

public class CM extends AbstractRomanNumeral {
    private static final CM INSTANCE = new CM();
    private static final int VALUE = 900;

    private CM() { }

    public static CM getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}