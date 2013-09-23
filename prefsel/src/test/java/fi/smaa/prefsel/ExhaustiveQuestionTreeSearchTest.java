package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class ExhaustiveQuestionTreeSearchTest {
	
	
	private AnswerNode root;

	@Before
	public void setUp() {
		RealMatrix imp = new Array2DRowRealMatrix(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		);
		
		root = ExhaustiveQuestionTreeSearch.buildTree(imp, new NullPreferenceModel());
	}

	@Test
	public void testBuildTreeRootHasNoAnswer() {
		assertEquals(AnswerNode.NO_ANSWER, root.getAnswer());		
	}
	
	@Test
	public void testTraversalWithLinearVFCut() {
		System.out.println("\n\n\n\n\n---");
		RealMatrix im = new Array2DRowRealMatrix(new double[][]{
				{1.0, 0.0},
				{0.0, 1.0},
				{0.2, 0.2}
		});
		TransitiveRelation prefs = new TransitiveRelation(3);
		LinearVFPreferenceModel p = new LinearVFPreferenceModel(im, prefs);
		root = ExhaustiveQuestionTreeSearch.buildTree(im, p);
		Node[] children = root.getChildren();
		assertEquals(0, children[0].getChildren()[0].getChildren().length);
		assertEquals(2, children[1].getChildren()[0].getChildren().length);
	}
	
	@Test
	public void testBuildTreeStructureOk() {
		Node[] qs = root.getChildren();
		assertEquals(3, qs.length);
		
		// first level nodes
		QuestionNode n1 = (QuestionNode) qs[0];
		QuestionNode n2 = (QuestionNode) qs[1];		
		QuestionNode n3 = (QuestionNode) qs[2];
		
		assertEquals(0, n1.getQuestion().getA1());
		assertEquals(1, n1.getQuestion().getA2());
		
		assertEquals(0, n2.getQuestion().getA1());
		assertEquals(2, n2.getQuestion().getA2());

		assertEquals(1, n3.getQuestion().getA1());
		assertEquals(2, n3.getQuestion().getA2());
		
		// second level nodes
		AnswerNode n1l = n1.getLeftChild();
		AnswerNode n1r = n1.getRightChild();
		
		AnswerNode n2l = n2.getLeftChild();
		AnswerNode n2r = n2.getRightChild();
		
		AnswerNode n3l = n3.getLeftChild();
		AnswerNode n3r = n3.getRightChild();
		
		assertEquals(0, n1l.getAnswer());
		assertEquals(1, n1r.getAnswer());
		
		assertEquals(0, n2l.getAnswer());
		assertEquals(2, n2r.getAnswer());

		assertEquals(1, n3l.getAnswer());
		assertEquals(2, n3r.getAnswer());
		
		// third level nodes - just test the first few, the rest should work out as it's done recursively
		Node[] n1lchildren = n1l.getChildren();
		assertEquals(2, n1lchildren.length);
		assertEquals(new Question(0, 2), ((QuestionNode) n1lchildren[0]).getQuestion());
		assertEquals(new Question(1, 2), ((QuestionNode) n1lchildren[1]).getQuestion());

		Node[] n1rchildren = n1r.getChildren();
		assertEquals(2, n1rchildren.length);
		assertEquals(new Question(0, 2), ((QuestionNode) n1rchildren[0]).getQuestion());
		assertEquals(new Question(1, 2), ((QuestionNode) n1rchildren[1]).getQuestion());
	}
	
	@Test
	public void testDominanceCuts() {
		AnswerNode n = (AnswerNode) root.getChildren()[0].getChildren()[0].getChildren()[0].getChildren()[1];
		assertEquals(0, n.getChildren().length);
	}
}
