package fi.smaa.prefsel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.apache.commons.math3.linear.RealMatrix;

public class AnswerNode implements Node {

	private int answer;
	private QuestionNode[] children;
	private TransitiveRelation currentPreferences;
	private PreferenceModel prefModel;
	private RealMatrix impacts;

	public static final int NO_ANSWER = -1;

	public AnswerNode(Question[] remainingQuestions, int answer, TransitiveRelation prefs, PreferenceModel prefModel, RealMatrix impacts) {
		this.prefModel = prefModel;
		this.answer = answer;
		this.currentPreferences = prefs;
		this.impacts = impacts;
		createChildren(remainingQuestions);
	}
	
	/**
	 * Build a root node (no answer)
	 * 
	 * @param questions
	 */
	public AnswerNode(Question[] questions, int nrAlts, PreferenceModel prefModel, RealMatrix impacts) {
		answer = NO_ANSWER;
		currentPreferences = new TransitiveRelation(nrAlts);
		this.impacts = impacts;
		this.prefModel = prefModel;
		createChildren(questions);
	}


	private void createChildren(Question[] remainingQuestions) {
		children = new QuestionNode[remainingQuestions.length];
		System.out.println("creating children for " + this + " with remaining questions " + Arrays.toString(remainingQuestions));
		for (int i=0;i<remainingQuestions.length;i++) {
			children[i] = new QuestionNode(remainingQuestions[i], cloneArrayWithoutOne(remainingQuestions, i), currentPreferences, prefModel, impacts);
		}		
	}

	private Question[] cloneArrayWithoutOne(Question[] remainingQuestions, int i) {
		ArrayList<Question> qs = new ArrayList<Question>();
		Collections.addAll(qs, remainingQuestions);
		qs.remove(i);
		return qs.toArray(new Question[0]);
	}

	public int getAnswer() {
		return answer;
	}

	public Node[] getChildren() {
		Node[] ch = new Node[children.length];
		for (int i=0;i<children.length;i++) {
			ch[i] = (Node) children[i];
		}
		return ch;
	}

	@Override
	public String toString() {
		return "a" + Integer.toString(answer);
	}
	
}