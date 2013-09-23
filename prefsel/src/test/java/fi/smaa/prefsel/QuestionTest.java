package fi.smaa.prefsel;

import junit.framework.Assert;

import org.junit.Test;

public class QuestionTest {

	@Test
	public void testEquals() {
		Assert.assertEquals(new Question(1, 2), new Question(1, 2));
		Assert.assertFalse(new Question(1, 2).equals(new Question(2, 2)));
		Assert.assertFalse(new Question(2, 2).equals(new Question(2, 1)));
	}
}
