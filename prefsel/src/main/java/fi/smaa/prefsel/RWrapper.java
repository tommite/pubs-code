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
	private static final double EPS = 1E-10;
	public static String SCRIPT_FILE = "/compute.metrics.R";
	
	private double hDVF;
	private RCaller caller;
	private RCode code;
	private RealMatrix measurements;
	private String script;
	private double[][] constr;
	
	public RWrapper(TransitiveAntisymmetricIrreflexiveRelation preferences, RealMatrix im) {
		this.measurements = im;
		caller = new RCaller();
		code = new RCode();
		caller.setRExecutable(R_EXECUTABLE);
		try {
			script = loadSource(SCRIPT_FILE);
		} catch (IOException e) {
			throw new IllegalStateException("error loading file: " + e.getMessage());
		} catch (URISyntaxException e) {
			throw new IllegalStateException("error in URI syntax - this should not happen!");
		}
		this.constr = createConstraints(preferences, im);

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
		code.clear();
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

	private static double[][] createConstraints(TransitiveAntisymmetricIrreflexiveRelation preferences, RealMatrix im) {
		int nrConstrs = preferences.getTrueCount();
		if (nrConstrs > 0) {
			RealMatrix constr = new Array2DRowRealMatrix(nrConstrs, im.getColumnDimension());
		
			int row = 0;
			for (Pair p : preferences.iterator()) {
				if (p.getFirst() != p.getSecond()) {
					RealVector diff = im.getRowVector(p.getSecond()).subtract(im.getRowVector(p.getFirst()));
					for (int i=0;i<diff.getDimension();i++) {
						diff.addToEntry(i, EPS);
					}
					if (row >= nrConstrs) {
						throw new IllegalStateException("Too large row, " + 
								preferences.getTrueCount() + " " + im.getRowDimension());
					}
					constr.setRowVector(row, diff);
					row++;
				}
			}
			if (row != nrConstrs) {
				throw new IllegalStateException("row 1= nrConstrs");
			}
			return constr.getData();
		} else {
			return null;
		}
	}
}
