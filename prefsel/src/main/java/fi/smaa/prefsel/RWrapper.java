package fi.smaa.prefsel;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;

import rcaller.RCaller;
import rcaller.RCode;

public class RWrapper {
	
	private static String SCRIPT_FILE = "R/compute.metrics.R";
	private double hDVF;
	
	public RWrapper(TransitiveAntisymmetricRelation preferences, RealMatrix measurements) {
		computeMetrics(preferences, measurements);
	}
	
	private String loadSource(String sourceFile) throws URISyntaxException, IOException {
		File src = new File( this.getClass().getResource(sourceFile).toURI() );
		return FileUtils.readFileToString(src);
	}

	/**
	 * 
	 * @param preferences
	 * @param im Impact matrix (measurements)
	 * @throws IOException 
	 * @throws URISyntaxException 
	 */
	private void computeMetrics(TransitiveAntisymmetricRelation preferences, RealMatrix im) {
		RCaller caller = new RCaller();	
		RCode code = new RCode();
		 
		try {
			code.addRCode(loadSource(SCRIPT_FILE));
		} catch (Exception e) {
			throw new IllegalStateException("error loading file");
		}
		
		double[][] constr = createConstraints(preferences, im);
		code.addDoubleMatrix("constraints", constr);
		caller.setRCode(code);
		caller.runAndReturnResult("metrics");
		hDVF = caller.getParser().getAsDoubleArray("hDVF")[0];
	}
	
	public double getHVDF() {
		return hDVF;
	}

	private double[][] createConstraints(TransitiveAntisymmetricRelation preferences, RealMatrix im) {
		RealMatrix constr = new Array2DRowRealMatrix(preferences.getTrueCount(), preferences.getDim());
		
		int row = 0;
		for (Pair p : preferences.iterator()) {
			RealVector diff = im.getRowVector(p.getFirst()).subtract(im.getColumnVector(p.getSecond()));
			constr.setRowVector(row, diff);
			row++;
		}
		return constr.getData();
	}
}
