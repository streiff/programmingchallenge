package com.rontologist.solium.romannumerals.server.servlet;

import com.rontologist.solium.romannumerals.common.ejb.NumberRemote;
import com.rontologist.solium.romannumerals.server.ejb.NumberRemoteServiceLocator;
import com.rontologist.solium.romannumerals.server.number.RomanNumber;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

public class NumberConverter extends HttpServlet {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	super.doGet(request, response);

	String sourceType = request.getParameter("sourceType");
	String destType = request.getParameter("destType");
	String number = request.getParameter("number");

	ByteArrayOutputStream out = new ByteArrayOutputStream();
	PrintWriter errorWriter = new PrintWriter(out);
	if (!isValid(sourceType, destType, number, errorWriter)) {
	    response.setStatus(500);
	    response.getOutputStream().write(out.toByteArray());
	    return;
	}

	PrintWriter writer = response.getWriter();
	RomanNumber romanNumber;
	try {
	    romanNumber = getNumberRemote().getRomanNumber(number);
	} catch (Exception e) {
	    writer.println(e.getMessage());
	    return;
	}

	writer.print(romanNumber.getRepresentation());
    }

    private boolean isValid(String sourceType, String destType, String number, PrintWriter errorWriter) {
	String errors = "";
	if (isNullOrBlank(sourceType)) {
	    errors += "Required parameter sourceType not given.\n";
	}
	if (isNullOrBlank(destType)) {
	    errors += "Required parameter destType not given.\n";
	}

	if (isNullOrBlank(number)) {
	    errors += "Required parameter number not given.\n";
	}

	if (isNullOrBlank(errors)) {
		return false;
	} else {
	    errorWriter.println(errors);
	    return true;
	}
    }

    private boolean isNullOrBlank(String s) {
	return s == null || s.trim().length() == 0;
    }

    public NumberRemote getNumberRemote() {
	return NumberRemoteServiceLocator.getInstance().getNumberRemote();
    }
}
