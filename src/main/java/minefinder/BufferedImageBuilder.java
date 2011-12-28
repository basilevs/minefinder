package minefinder;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

/**
 * @author Anthony Eden
 */
public class BufferedImageBuilder {

    private static final int DEFAULT_IMAGE_TYPE = BufferedImage.TYPE_INT_RGB;

    public BufferedImage bufferImage(Image image) {
		if (image instanceof BufferedImage){
			return (BufferedImage) image;
		}
        return bufferImage(image, DEFAULT_IMAGE_TYPE);
    }

    public BufferedImage bufferImage(Image image, int type) {
//		new ImageIcon(image).getImage()
        BufferedImage bufferedImage = new BufferedImage(image.getWidth(null), image.getHeight(null), type);
        Graphics2D g = bufferedImage.createGraphics();
        g.drawImage(image, null, null);
        waitForImage(bufferedImage);
        return bufferedImage;
    }

    private void waitForImage(BufferedImage bufferedImage) {
        final ImageLoadStatus imageLoadStatus = new ImageLoadStatus();
        if (bufferedImage.getHeight(new ImageObserver() {
            public boolean imageUpdate(Image img, int infoflags, int x, int y, int width, int height) {
                if (infoflags == ALLBITS) {
                    imageLoadStatus.heightDone = true;
                    return true;
                }
                return false;
            }
        }) >= 0){
			imageLoadStatus.heightDone = true;
		}
        if (bufferedImage.getWidth(new ImageObserver() {
            public boolean imageUpdate(Image img, int infoflags, int x, int y, int width, int height) {
                if (infoflags == ALLBITS) {
                    imageLoadStatus.widthDone = true;
                    return true;
                }
                return false;
            }
        }) >= 0){
			imageLoadStatus.widthDone = true;
		}
      while (!imageLoadStatus.widthDone && !imageLoadStatus.heightDone) {
            try {
                Thread.sleep(300);
            } catch (InterruptedException e) {

            }
        }
    }

    class ImageLoadStatus {

        public boolean widthDone = false;
        public boolean heightDone = false;
    }

}