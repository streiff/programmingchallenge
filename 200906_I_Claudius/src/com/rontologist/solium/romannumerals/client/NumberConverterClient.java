package com.rontologist.solium.romannumerals.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.HttpURLConnection;
import java.net.URL;

/** A simple client that does a REST webservice call to a server to convert the
 *  given decimal numeral number into roman numerals.
 */

public class NumberConverterClient {

    public static void main(String args[]) {

	if (args.length != 0) {
	    printUsage(System.err);
	    return;
	}

	int number;
	try {
	    number = Integer.parseInt(args[0]);
	} catch (NumberFormatException e) {
	    System.err.println("Error: argument not a number");
	    printUsage(System.err);
	    return;
	}

	if (number < 1 || number > 3999) {
	    System.err.print("Number must be between 1 and 3999 inclusive");
	    return;
	}

	try {
	    String romanNumeral = doRESTCall(number, System.err);
	    if (romanNumeral != null) {
		System.out.println("The roman numeral version of " + number + " is " + romanNumeral);
	    }
	} catch (IOException e) {
	    System.err.println("There was an error with the converesion.");
	    e.printStackTrace();
	}
    }

    /** Performs a REST webservice call.
     *
     * @param number Number to convert
     * @param err The printstream used to output error messages to the user
     * @return A string representation of the roman numeral, or null if it could not be converted
     * @throws IOException An error occurred while talking to the webserver.
     */
    private static String doRESTCall(int number, PrintStream err) throws IOException {
	Config config = new Config();

	URL serviceURL = new URL("http://" + config.getHost() + "/"+ config.getService() + "?sourceType=decimal&destType=roman&number=" + number);
	HttpURLConnection conn = (HttpURLConnection) serviceURL.openConnection();
	int statusCode = conn.getResponseCode();

	InputStream in = conn.getInputStream();
	BufferedReader reader = new BufferedReader(new InputStreamReader(in));

	if (HttpURLConnection.HTTP_INTERNAL_ERROR == statusCode) {
	    err.println("There was an error with the conversion. Please see errors below.\n");
	    String inputLine;
	    while((inputLine = reader.readLine()) != null) {
		err.println(inputLine);
	    }
	    in.close();
	    return null;
	} else {
	    return reader.readLine();
	}
    }

    /** Prints the usage to the user.
     * @param out The printstream used to output the message to the user.  
     */
    private static void printUsage(PrintStream out) {
	out.println("Usage: NumberConverterClient [decimal_number]");
    }
}
