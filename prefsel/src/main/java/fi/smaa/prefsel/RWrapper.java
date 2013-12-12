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
	
	private static final String R_EXECUTABLE = "/usr/bin/R";
	public static String SCRIPT_FILE = "/compute.metrics.R";
	
	private double hDVF;
	private RCaller caller;
	private RCode code;
	private double[][] constr;
	private RealMatrix measurements;
	private String script;
	
	public RWrapper(TransitiveAntisymmetricRelation preferences, RealMatrix measurements) {
		this.measurements = measurements;
		initR(preferences, measurements);
	}
	
	private void initR(TransitiveAntisymmetricRelation preferences, RealMatrix im) {
		caller = new RCaller();
		code = new RCode();
		caller.setRExecutable(R_EXECUTABLE);		
		constr = createConstraints(preferences, im);
		try {
			script = loadSource(SCRIPT_FILE);
		} catch (IOException e) {
			throw new IllegalStateException("error loading file: " + e.getMessage());
		} catch (URISyntaxException e) {
			throw new IllegalStateException("error in URI syntax - this should not happen!");
		}		
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
	public void computeMetrics(int a1, int a2) {
		caller.cleanRCode();
		if (constr != null) {
			code.addDoubleMatrix("constraints", constr);
		}
		code.addDoubleMatrix("performances", measurements.getData());
		code.addIntArray("pair", new int[] {a1+1, a2+1});

		code.addRCode(script);
		caller.setRCode(code);
		caller.runAndReturnResultOnline("results");
		double[] hdvfArr = caller.getParser().getAsDoubleArray("hDVF");
		if (hdvfArr.length != 1) {
			throw new IllegalStateException("R script return array not length 1");
		}
		hDVF = hdvfArr[0];
	}
	
	public double getHVDF() {
		return hDVF;
	}

	// package public to enable testing
	private double[][] createConstraints(TransitiveAntisymmetricRelation preferences, RealMatrix im) {
		int nrConstrs = preferences.getTrueCount() - im.getRowDimension();
		if (nrConstrs > 0) {
			RealMatrix constr = new Array2DRowRealMatrix(nrConstrs, im.getColumnDimension());
		
			int row = 0;
			for (Pair p : preferences.iterator()) {
				if (p.getFirst() != p.getSecond()) {
					RealVector diff = im.getRowVector(p.getSecond()).subtract(im.getRowVector(p.getFirst()));
					constr.setRowVector(row, diff);
					row++;
				}
			}
			return constr.getData();
		} else {
			return null;
		}
	}
}
