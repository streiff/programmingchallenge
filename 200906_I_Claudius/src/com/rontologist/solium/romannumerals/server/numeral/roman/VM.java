package com.rontologist.solium.romannumerals.server.numeral.roman;

public class VM extends AbstractRomanNumeral {
    private static final VM INSTANCE = new VM();
    private static final int VALUE = 995;

    private VM() { }

    public static VM getInstance() {
        return INSTANCE;
    }

    public int getValue() {
        return VALUE;
    }
}