package com.rontologist.solium.romannumerals.server.numeral.roman;

import com.rontologist.solium.romannumerals.server.numeral.NumeralFactory;

import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class RomanNumeralFactory implements NumeralFactory<RomanNumeral> {
    private static final RomanNumeralFactory INSTANCE = new RomanNumeralFactory();
    private final Map<Integer, RomanNumeral> LOOKUP_MAP;

    private RomanNumeralFactory() {
        Map<Integer, RomanNumeral> map = new HashMap<Integer, RomanNumeral>();
        addToMap(map, C.getInstance());
        addToMap(map, CD.getInstance());
        addToMap(map, CM.getInstance());
        addToMap(map, D.getInstance());
        addToMap(map, I.getInstance());
        addToMap(map, IV.getInstance());
        addToMap(map, IX.getInstance());
        addToMap(map, L.getInstance());
        addToMap(map, M.getInstance());
        addToMap(map, V.getInstance());
        addToMap(map, X.getInstance());
        addToMap(map, XC.getInstance());
        addToMap(map, XL.getInstance());
        LOOKUP_MAP = Collections.unmodifiableMap(map);
    }

    public static RomanNumeralFactory getInstance() {
        return INSTANCE;
    }

    public RomanNumeral getNumeral(int value) {
        return LOOKUP_MAP.containsKey(value) ? LOOKUP_MAP.get(value) : null;
    }

    private void addToMap(Map<Integer, RomanNumeral> map, RomanNumeral n) {
        map.put(n.getValue(), n);
    }
}
