package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix2D;

public class ExhaustiveQuestionTreeSearchTest {
	
	
	private AnswerNode root;

	@Before
	public void setUp() {
		DoubleMatrix2D imp = DoubleFactory2D.dense.make(new double[][]{
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
	public void testBuildTreeStructureOk() {
		QuestionNode[] qs = root.getChildren();
		assertEquals(3, qs.length);
		
		// first level nodes
		QuestionNode n1 = qs[0];
		QuestionNode n2 = qs[1];		
		QuestionNode n3 = qs[2];
		
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
		QuestionNode[] n1lchildren = n1l.getChildren();
		assertEquals(2, n1lchildren.length);
		assertEquals(new Question(0, 2), n1lchildren[0].getQuestion());
		assertEquals(new Question(1, 2), n1lchildren[1].getQuestion());

		QuestionNode[] n1rchildren = n1r.getChildren();
		assertEquals(2, n1rchildren.length);
		assertEquals(new Question(0, 2), n1rchildren[0].getQuestion());
		assertEquals(new Question(1, 2), n1rchildren[1].getQuestion());


	}
}
